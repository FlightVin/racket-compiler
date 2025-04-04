**1. Overview**

The LLRacket compiler employs a static type checking system implemented primarily within the Semantic Analysis (`Sema`) phase. This phase runs after parsing (which generates the Abstract Syntax Tree - AST) and before code generation (`CodeGen`). Its main goals are:

*   **Determine the static type** of every expression (`Expr`) node in the AST.
*   **Verify type consistency** according to the language rules (e.g., operands of `+` must be integers, condition of `if` must be boolean).
*   **Report type errors** clearly to the user using the `DiagnosticsEngine`.
*   **Store the determined types** so the `CodeGen` phase can use them to generate type-correct LLVM Intermediate Representation (IR).

**2. Type System Definition (`include/llracket/Basic/Type.h`)**

The foundation is a simple enumeration `ExprType` defining the possible static types in the language:

*   `ExprType::Integer`: Represents integer values.
*   `ExprType::Boolean`: Represents boolean values (`#t`, `#f`).
*   `ExprType::Void`: Represents expressions that produce no value (like `set!`, `while`, `(void)`).
*   `ExprType::Error`: A special type assigned when a type error is detected or a type cannot be determined. This helps prevent cascading errors and signals issues to later phases.

A helper function `getTypeName` converts these enum values into human-readable strings for error messages.

**3. Core Mechanism: `Sema` and `TypeCheckVisitor` (`lib/Sema/Sema.cpp`)**

*   **`Sema` Class (`include/llracket/Sema/Sema.h`):** This class orchestrates the semantic analysis.
    *   It holds a reference to the `DiagnosticsEngine` (`Diags`) for reporting errors.
    *   It owns the primary data structure for storing results: `llvm::DenseMap<Expr *, ExprType> ExprTypes`. This map associates each AST expression node (`Expr *`) with its determined `ExprType`.
    *   It uses `llvm::StringMap<ExprType> CurrentVarTypes` to keep track of the types of variables currently in scope.
    *   The main entry point is `typeCheck(AST *Tree)`, which returns `true` on success (no errors) and `false` otherwise.

*   **`TypeCheckVisitor` Class (`lib/Sema/Sema.cpp`):** The actual type checking logic is implemented using the Visitor pattern within this internal class.
    *   It traverses the AST recursively.
    *   It inherits from `ASTVisitor` and overrides the `visit` method for each specific AST node type (`Int`, `Bool`, `Var`, `Let`, `Prim`, `If`, `SetBang`, `Begin`, `WhileLoop`, `Void`, etc.).
    *   It uses the `ExprTypes` map (passed by reference from `Sema`) to store the type it determines for each node it visits.
    *   It uses the `CurrentVarTypes` map (also passed by reference) to resolve variable types and manage scopes (especially for `Let`).
    *   It has helper methods (`recordType`, `reportTypeError`, `reportExpectedTypeError`, `reportError`) to store types and report errors via the `Diags` engine.
    *   A `HasError` flag tracks if any error occurred during the visitor's traversal.

**4. Type Checking and Assignment Rules (Inside `TypeCheckVisitor`)**

The `visit` method for each `Expr` subtype implements the specific type rules:

*   **Literals (`visit(Int &)`, `visit(Bool &)`, `visit(Void &)`):** These are the base cases. They simply record their inherent type (`Integer`, `Boolean`, `Void`) in the `ExprTypes` map.
*   **Variables (`visit(Var &)`):**
    *   Looks up the variable's name (`Node.getName()`) in the `CurrentVarTypes` map.
    *   If found, it records the retrieved type for the `Var` node.
    *   If not found, it reports an `err_undefined_variable` and records `ExprType::Error`.
*   **Let Expressions (`visit(Let &)`):**
    *   Recursively determines the type of the `Binding` expression.
    *   **Scope Management:** Saves the current type (if any) associated with `VarName` in `CurrentVarTypes`.
    *   Updates `CurrentVarTypes` by mapping `VarName` to the `Binding`'s type (if it wasn't `Error`).
    *   Recursively determines the type of the `Body` expression within this new scope.
    *   **Scope Restoration:** Restores the original type mapping for `VarName` in `CurrentVarTypes`.
    *   The type of the `Let` expression itself is the type of its `Body`. If the `Binding` had an error, the `Let`'s type is `Error`. Records this result.
*   **Primitives (`visit(Prim &)`):**
    *   Recursively determines the types of operands (`E1`, `E2` if present).
    *   Checks if operand types match the requirements of the operator (`Op`):
        *   `+`, `-` (unary/binary): Expect `Integer`, result `Integer`. Reports `err_expected_type` on mismatch.
        *   Comparisons (`<`, `<=`, `>`, `>=`): Expect `Integer`, result `Boolean`. Reports `err_expected_type`.
        *   `eq?`: Expects both operands to have the *same* type, which must be `Integer` or `Boolean`. Result `Boolean`. Reports `err_type_mismatch` or `err_expected_type`.
        *   `not`: Expects `Boolean`, result `Boolean`. Reports `err_expected_type`.
        *   `and`, `or`: Expects `Boolean`, result `Boolean`. Reports `err_expected_type`.
        *   `read`: Takes no arguments, result `Integer`.
    *   Checks for the correct number of operands using `reportOperandCountError`.
    *   Records the calculated result type or `ExprType::Error` if any check fails.
*   **If Expressions (`visit(If &)`):**
    *   Recursively determines the types of the `Condition`, `ThenExpr`, and `ElseExpr`.
    *   Checks if the `Condition`'s type is `Boolean`. Reports `err_expected_type` if not.
    *   Checks if the `ThenExpr`'s type and `ElseExpr`'s type are identical (and not `Error`). Reports `err_if_branch_mismatch` if they differ.
    *   The type of the `If` expression is the common type of the branches *only if* the condition is `Boolean` and the branches match. Otherwise, the type is `ExprType::Error`. Records the result.
*   **Set! Expressions (`visit(SetBang &)`):**
    *   Checks if the variable (`VarName`) exists in `CurrentVarTypes`. Reports `err_set_undefined` if not.
    *   Recursively determines the type of the `ValueExpr`.
    *   If the variable exists, checks if the `ValueExpr`'s type matches the variable's declared type (from `CurrentVarTypes`). Reports `err_type_mismatch` if not.
    *   **Crucially, the result type of a `set!` expression is always `ExprType::Void`**, regardless of errors during analysis. Records `Void`.
*   **Begin Expressions (`visit(Begin &)`):**
    *   Recursively determines the type of each expression in the sequence.
    *   Checks if the sequence is empty (`err_empty_begin`).
    *   The type of the `Begin` expression is the type of the *last* expression in the sequence.
    *   If any expression within the sequence results in `ExprType::Error`, the entire `Begin` expression's type becomes `ExprType::Error`. Records the result.
*   **While Loops (`visit(WhileLoop &)`):**
    *   Recursively determines the types of the `Condition` and `Body`.
    *   Checks if the `Condition`'s type is `Boolean`. Reports `err_expected_type` if not.
    *   **The result type of a `while` expression is always `ExprType::Void`**. Records `Void`.

**5. Storing and Using Type Information**

*   After the `TypeCheckVisitor` finishes, the `Sema` object contains the populated `ExprTypes` map.
*   The driver (`tools/driver/LLRacket.cpp`) retrieves this map using `Sema::getExprTypes()`.
*   This map (`typeResults`) is then passed to the `CodeGen` constructor.
*   The `CodeGen` phase (specifically the `ToIRVisitor` in `lib/CodeGen/CodeGen.cpp`) uses this map extensively:
    *   To determine the correct LLVM `Type*` (e.g., `i32`, `i1`) for variables, expression results, and function arguments/returns using helpers like `getLLVMType`.
    *   To generate the correct LLVM instructions based on operand types (e.g., `CreateNSWAdd` for integers, `CreateICmpSLT` for integer comparison, `CreateAnd` for booleans).
    *   To insert explicit casts (`CreateZExt`, `CreateSExt`, `CreateICmpNE`) when necessary (e.g., converting `i1` condition results for branching, converting between `i1` and `i32` if needed by runtime functions or assignment).
    *   To select the correct runtime function for printing (`write_int` vs `write_bool`).
    *   To handle `ExprType::Error` defensively, often generating default values (like 0 or `false`) or skipping code generation for the erroneous parts.

**In Summary:**

The type checking process is a systematic traversal of the AST using a visitor pattern. It applies specific rules based on the language constructs to determine the type of each expression. It maintains variable type information through a symbol table (`CurrentVarTypes`) respecting lexical scope (`let`). The results are stored in a map (`ExprTypes`) associating AST nodes with their static types. This map is then consumed by the code generator to produce type-correct LLVM IR. The `DiagnosticsEngine` is used throughout to report any inconsistencies or errors found.