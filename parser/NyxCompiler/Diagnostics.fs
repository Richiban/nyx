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

    let errorAt message (line: int, column: int) =
        { Severity = ErrorSeverity
          Message = message
          Range = Some (line, column) }

    let warning message =
        { Severity = WarningSeverity
          Message = message
          Range = None }

    let warningAt message (line: int, column: int) =
        { Severity = WarningSeverity
          Message = message
          Range = Some (line, column) }
