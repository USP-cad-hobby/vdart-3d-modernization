# scripts/check_duplicate_modules.ps1
# Project: VDaRT
# Purpose: quick scan for duplicate module names across .f90 files
# Author: U.S.Paulsen
# Date: 2026-02-25

param(
  [string]$Root = "."
)

$modules = @()

Get-ChildItem -Path $Root -Filter "*.f90" -Recurse | ForEach-Object {
  $file = $_.FullName
  Select-String -Path $file -Pattern '^\s*module\s+([A-Za-z0-9_]+)\b' -SimpleMatch | ForEach-Object {
    $name = $_.Matches.Groups[1].Value
    $modules += [PSCustomObject]@{ Module = $name; File = $file }
  }
}

$grouped = $modules | Group-Object -Property Module
$duplicates = $grouped | Where-Object { $_.Count -gt 1 }

if ($duplicates.Count -eq 0) {
  Write-Host "No duplicate modules found."
  exit 0
}

Write-Host "Duplicate module names found:"
foreach ($g in $duplicates) {
  Write-Host "Module: $($g.Name)"
  $g.Group | ForEach-Object { Write-Host "  $_.File" }
}
exit 1