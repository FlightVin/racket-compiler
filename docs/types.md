# Type System Documentation

*LLM generated summaries of the LLRacket type system implementation.*

## 1. Overview

The LLRacket compiler employs a static type checking system implemented primarily within the Semantic Analysis (`Sema`) phase. This phase runs after parsing (which generates the Abstract Syntax Tree - AST) and before code generation (`CodeGen`). Its main goals are:

*   **Determine the static type** of every expression (`Expr`) node in the AST.
*   **Verify type consistency** according to the language rules (e.g., operands of `+` must be integers, condition of `if` must be boolean).
*   **Attempt type inference** where types are not immediately explicit (primarily involving `(read)`).
*   **Report type errors** clearly to the user using the `DiagnosticsEngine`.
*   **Store the determined types** so the `CodeGen` phase can use them to generate type-correct LLVM Intermediate Representation (IR).

**2. Type System Definition (`include/llracket/Basic/Type.h`)**

The foundation is a simple enumeration `ExprType` defining the possible static types in the language:

*   `ExprType::Integer`: Represents integer values.
*   `ExprType::Boolean`: Represents boolean values (`#t`, `#f`).
*   `ExprType::Void`: Represents expressions that produce no value (like `set!`, `while`, `(void)`, `begin` with no final value).
*   `ExprType::NeedsInference`: A temporary type assigned to expressions (currently only `(read)`) whose final type depends on the context in which they are used.
*   `ExprType::Error`: A special type assigned when a type error is detected or a type cannot be determined (e.g., undefined variable, failed inference). This helps prevent cascading errors and signals issues to later phases.

A helper function `getTypeName` converts these enum values into human-readable strings for error messages.

**3. Core Mechanism: `Sema` and `TypeCheckVisitor` (`lib/Sema/Sema.cpp`)**

*   **`Sema` Class (`include/llracket/Sema/Sema.h`):** This class orchestrates the semantic analysis.
    *   It holds a reference to the `DiagnosticsEngine` (`Diags`) for reporting errors.
    *   It owns the primary data structure for storing results: `llvm::DenseMap<Expr *, ExprType> ExprTypes`. This map associates each AST expression node (`Expr *`) with its determined `ExprType`.
    *   It uses `llvm::StringMap<ExprType> CurrentVarTypes` to keep track of the types of variables currently in scope.
    *   The main entry point is `typeCheck(AST *Tree)`, which returns `true` on success (no errors) and `false` otherwise.

*   **`TypeCheckVisitor` Class (`lib/Sema/Sema.cpp`):** The actual type checking logic is implemented using the Visitor pattern within this internal class.
    *   It traverses the AST recursively using a helper `visitAndGetType`.
    *   It inherits from `ASTVisitor` and overrides the `visit` method for each specific AST node type (`Int`, `Bool`, `Var`, `Let`, `Prim`, `If`, `SetBang`, `Begin`, `WhileLoop`, `Void`, etc.).
    *   It uses the `ExprTypes` map (passed by reference from `Sema`) to store the type it determines for each node it visits.
    *   It uses the `CurrentVarTypes` map (also passed by reference) to resolve variable types and manage scopes (especially for `Let`).
    *   It implements type inference logic, resolving `NeedsInference` based on context.
    *   It has helper methods (`recordType`, `reportTypeError`, `reportExpectedTypeError`, `reportError`) to store types and report errors via the `Diags` engine.
    *   A `HasError` flag tracks if any error occurred during the visitor's traversal.
    *   After traversal, it performs a final check (`checkUnresolvedTypes`) for any `NeedsInference` types that couldn't be resolved, reporting them as errors.

**4. Type Inference**

Because the `(read)` expression can conceptually return either an `Integer` or a `Boolean` depending on runtime input (and the type flag passed to the runtime `read_value` function), its static type must be inferred from the context where it's used.

*   **Initial Assignment:** When the `TypeCheckVisitor` encounters a `Prim` node representing `(read)`, it initially assigns it the type `ExprType::NeedsInference`.
*   **Contextual Resolution:** This `NeedsInference` type is resolved (changed to `Integer`, `Boolean`, or `Error`) when visiting the *parent* expression that uses the `(read)` result:
    *   **`let` Binding (`visit(Let &)`):** If `exp1` in `(let ([x exp1]) ...)` is `(read)`, the spec *guarantees* the input will be an integer. The visitor forces the type of `exp1` (and thus `x`) to `ExprType::Integer`. If `exp1` yields `NeedsInference` but isn't `(read)`, it's an error.
    *   **`if`/`while` Condition (`visit(If &)`, `visit(WhileLoop &)`):** If the condition expression evaluates to `NeedsInference`, it's inferred to be `ExprType::Boolean`.
    *   **Arithmetic/Comparison Operands (`visit(Prim &)` for `+`, `-`, `<`, `<=`, `>`, `>=`):** If an operand has type `NeedsInference`, it's inferred to be `ExprType::Integer`.
    *   **`eq?` Operands (`visit(Prim &)`):** If one operand is `NeedsInference` and the other is `Integer` or `Boolean`, the `NeedsInference` operand is inferred to match. If both are `NeedsInference`, an error (`err_cannot_infer_type`) is reported.
    *   **Logical Operands (`visit(Prim &)` for `and`, `or`, `not`):** If an operand is `NeedsInference`, it's inferred to be `ExprType::Boolean`.
    *   **`set!` Value (`visit(SetBang &)`):** If the value expression yields `NeedsInference`, its type is inferred from the already-known type of the variable being assigned to.
*   **Final Check:** If, after checking the entire AST, any expression still has the type `NeedsInference`, it means the context was insufficient to determine the type, and an `err_cannot_infer_type` error is reported.

**5. Type Checking and Assignment Rules (Inside `TypeCheckVisitor`)**

The `visit` method for each `Expr` subtype implements the specific type rules, incorporating inference:

*   **Literals (`visit(Int &)`, `visit(Bool &)`, `visit(Void &)`):** Assign their inherent types (`Integer`, `Boolean`, `Void`).
*   **Variables (`visit(Var &)`):** Looks up type in `CurrentVarTypes`. Reports `err_undefined_variable` or assigns the found type.
*   **Let Expressions (`visit(Let &)`):**
    *   Determines/infers binding type (handling `(read)` as `Integer`).
    *   Updates scope, determines body type, restores scope.
    *   Result type is the body's type (or `Error`).
*   **Primitives (`visit(Prim &)`):**
    *   Determines/infers operand types based on the operator.
    *   Reports errors (`err_expected_type`, `err_type_mismatch`, `err_wrong_operand_count`) if checks fail after inference.
    *   **(read):** Assigns `NeedsInference`.
    *   `+`, `-`: Operands inferred/checked as `Integer`. Result `Integer`.
    *   Comparisons: Operands inferred/checked as `Integer`. Result `Boolean`.
    *   `eq?`: Operands inferred/checked to be identical `Integer` or `Boolean`. Result `Boolean`.
    *   `not`, `and`, `or`: Operands inferred/checked as `Boolean`. Result `Boolean`.
    *   Records result type or `Error`.
*   **If Expressions (`visit(If &)`):**
    *   Infers/checks condition type is `Boolean`.
    *   Infers/checks branch types are identical.
    *   Result type is the common branch type (or `Error`).
*   **Set! Expressions (`visit(SetBang &)`):**
    *   Checks variable exists.
    *   Determines/infers value type, matching it against the variable's type.
    *   Result type is always `Void`.
*   **Begin Expressions (`visit(Begin &)`):**
    *   Determines type of each sub-expression.
    *   Result type is the type of the *last* expression (or `Error` if any sub-expression had an error or `begin` was empty).
*   **While Loops (`visit(WhileLoop &)`):**
    *   Infers/checks condition type is `Boolean`.
    *   Result type is always `Void`.

**6. Storing and Using Type Information**

*   After the `TypeCheckVisitor` finishes, the `Sema` object contains the populated `ExprTypes` map.
*   The driver (`tools/driver/LLRacket.cpp`) retrieves this map using `Sema::getExprTypes()`.
*   This map (`typeResults`) is then passed to the `CodeGen` constructor.
*   The `CodeGen` phase (specifically the `ToIRVisitor` in `lib/CodeGen/CodeGen.cpp`) uses this map extensively:
    *   To determine the correct LLVM `Type*` (e.g., `i32`, `i1`) for variables, expression results, and function arguments/returns using helpers like `getLLVMType`.
    *   To generate the correct LLVM instructions based on operand types (e.g., `CreateNSWAdd` for integers, `CreateICmpSLT` for integer comparison, `CreateAnd` for booleans).
    *   To insert explicit casts (`CreateZExt`, `CreateSExt`, `CreateICmpNE`) when necessary (e.g., converting `i1` condition results for branching, converting between `i1` and `i32` if needed by runtime functions or assignment).
    *   To select the correct runtime function for printing (`write_int` vs `write_bool`).
    *   To determine the correct type flag (0 or 1) to pass to the `read_value` runtime function based on the resolved type of the `(read)` expression.
    *   To handle `ExprType::Error` defensively, often generating default values (like 0 or `false`) or skipping code generation for the erroneous parts.

**In Summary:**

The type checking process is a systematic traversal of the AST using a visitor pattern. It applies specific rules based on the language constructs to determine the type of each expression, employing context-based **type inference** to resolve the ambiguous type of `(read)`. It maintains variable type information through a symbol table (`CurrentVarTypes`) respecting lexical scope (`let`). The results are stored in a map (`ExprTypes`) associating AST nodes with their static types. This map is then consumed by the code generator to produce type-correct LLVM IR. The `DiagnosticsEngine` is used throughout to report any inconsistencies or errors found.