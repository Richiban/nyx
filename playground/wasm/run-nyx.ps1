param(
    [Parameter(Position = 0)]
    [string]$NyxFile = "./twelve.nyx",

    [Parameter(Position = 1)]
    [string]$ExportName = "main",

    [Parameter(Position = 2, ValueFromRemainingArguments = $true)]
    [string[]]$ExportArgs = @()
)

$nyxPath = Resolve-Path $NyxFile -ErrorAction Stop
$watPath = [System.IO.Path]::ChangeExtension($nyxPath.Path, ".wat")
$compilerProject = Join-Path $PSScriptRoot "../../compiler/Nyx.Compiler.Cli/NyxCompiler.Cli.fsproj"

Write-Host "Compiling $($nyxPath.Path) -> $watPath"
dotnet run --project $compilerProject -- $nyxPath.Path --target wasm --out $watPath
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

Write-Host "Running $watPath (export: $ExportName)"
node (Join-Path $PSScriptRoot "run-wat.mjs") $watPath $ExportName @ExportArgs
exit $LASTEXITCODE
