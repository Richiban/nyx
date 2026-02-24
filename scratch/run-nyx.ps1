param(
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$NyxFile,

    [Parameter(Position = 1)]
    [string]$ExportName = "main",

    [Parameter(Position = 2, ValueFromRemainingArguments = $true)]
    [string[]]$ExportArgs = @()
)

$nyxPath = Resolve-Path $NyxFile -ErrorAction Stop
$watPath = [System.IO.Path]::ChangeExtension($nyxPath.Path, ".wat")
$compilerProject = Join-Path $PSScriptRoot "../compiler/Nanyx.Compiler.Cli/NyxCompiler.Cli.fsproj"

Write-Host "Compiling $($nyxPath.Path) -> $watPath"
dotnet run --project $compilerProject -- $nyxPath.Path --target wasm --out $watPath
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

Write-Host "Running $watPath (export: $ExportName)"

$wasmtime = Get-Command wasmtime -ErrorAction SilentlyContinue
if (-not $wasmtime) {
    Write-Error "wasmtime was not found in PATH. Install it or add it to PATH to run .wat files."
    exit 1
}

$args = @()
if ($ExportName) {
    $args += @("--invoke", $ExportName)
}
if ($ExportArgs.Count -gt 0) {
    $args += $ExportArgs
}
$args += $watPath

& $wasmtime.Source $args
exit $LASTEXITCODE
