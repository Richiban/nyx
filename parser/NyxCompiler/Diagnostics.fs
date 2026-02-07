namespace NyxCompiler

type Severity =
    | ErrorSeverity
    | WarningSeverity

type Diagnostic =
    { Severity: Severity
      Message: string
      Range: (int * int) option }

module Diagnostics =
    let error message =
        { Severity = ErrorSeverity
          Message = message
          Range = None }

    let warning message =
        { Severity = WarningSeverity
          Message = message
          Range = None }
