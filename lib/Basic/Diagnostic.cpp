#include "llracket/Basic/Diagnostic.h"
#include <llvm/ADT/StringRef.h>

using namespace llracket;

namespace {
const char *DiagnosticText[] = {

#define DIAG(ID, Level, Msg) Msg,
#include "llracket/Basic/Diagnostic.def"

};

llvm::SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "llracket/Basic/Diagnostic.def"
};

} // namespace
const char *DiagnosticsEngine::getDiagnosticText(unsigned DiagID) {
  // Add bounds check if necessary
  if (DiagID >= sizeof(DiagnosticText) / sizeof(DiagnosticText[0])) {
    return "Invalid diagnostic ID";
  }
  return DiagnosticText[DiagID];
}
llvm::SourceMgr::DiagKind
DiagnosticsEngine::getDiagnosticKind(unsigned DiagID) {
  // Add bounds check if necessary
   if (DiagID >= sizeof(DiagnosticKind) / sizeof(DiagnosticKind[0])) {
    return llvm::SourceMgr::DK_Error; // Default to error for invalid ID
  }
  return DiagnosticKind[DiagID];
}