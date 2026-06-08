param(
    [string]$ExpectedEmail = "sihan.zhuang@student.adelaide.edu.au",
    [string]$ExpectedCredentialUser = "Sihan324",
    [string]$ExpectedRemoteFragment = "Sihan324/sihan",
    [switch]$AllowDirty
)

$ErrorActionPreference = "Stop"

function Invoke-Git {
    param([string[]]$Arguments)
    $repoPath = (Get-Location).Path
    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
        return (& git -c "safe.directory=$repoPath" @Arguments 2>$null)
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }
}

function Get-GitConfigValue {
    param([string[]]$Arguments)
    $gitArguments = @("config") + $Arguments
    $value = Invoke-Git $gitArguments 2>$null
    if ($LASTEXITCODE -ne 0 -or $null -eq $value) {
        return ""
    }
    return ($value -join "`n").Trim()
}

function Test-PathNonEmpty {
    param([string]$Path)
    if (-not (Test-Path -LiteralPath $Path)) {
        return $false
    }
    return ((Get-Item -LiteralPath $Path).Length -gt 0)
}

function Add-Check {
    param(
        [System.Collections.Generic.List[object]]$Checks,
        [string]$Name,
        [bool]$Passed,
        [string]$Detail
    )
    $Checks.Add([pscustomobject]@{
        check  = $Name
        passed = $Passed
        detail = $Detail
    })
}

$checks = [System.Collections.Generic.List[object]]::new()

$gitEmail = Get-GitConfigValue @("user.email")
$gitName = Get-GitConfigValue @("user.name")
$credentialUser = Get-GitConfigValue @("--local", "credential.username")
$remote = (Invoke-Git @("remote", "get-url", "origin") 2>$null)
if ($LASTEXITCODE -ne 0 -or $null -eq $remote) {
    $remote = ""
}
else {
    $remote = ($remote -join "`n").Trim()
}

Add-Check $checks "git_email" ($gitEmail -eq $ExpectedEmail) $gitEmail
Add-Check $checks "git_name" ($gitName -eq "Sihan Zhuang") $gitName
Add-Check $checks "credential_username" ($credentialUser -eq $ExpectedCredentialUser) $credentialUser
Add-Check $checks "remote_repository" ($remote -like "*$ExpectedRemoteFragment*") $remote

$status = (Invoke-Git @("status", "--short"))
$workingTreeClean = [string]::IsNullOrWhiteSpace($status)
Add-Check $checks "working_tree_clean" ($workingTreeClean -or $AllowDirty.IsPresent) (($status -join "; ") -replace "`r?`n", "; ")

$rscript = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
Add-Check $checks "rscript_exists" (Test-Path -LiteralPath $rscript) $rscript

$quarto = Get-Command quarto -ErrorAction SilentlyContinue
$rstudioPandoc = "C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools\pandoc.exe"
$quartoDetail = if ($quarto) { $quarto.Source } else { "not found on PATH; render from RStudio/Positron or install Quarto CLI" }
Add-Check $checks "quarto_path_note" $true $quartoDetail
Add-Check $checks "rstudio_bundled_pandoc" (Test-Path -LiteralPath $rstudioPandoc) $rstudioPandoc

$requiredFiles = @(
    "report/final_report.qmd",
    "report/statistical_appendix.qmd",
    "R/run_all.R",
    "R/07_submission_audit.R",
    "data/processed/report_tables/submission_audit_summary.csv",
    "notes/final_review_checklist.md",
    "notes/final_polishing_notes.md",
    "notes/model_interpretation_caveats.md"
)

foreach ($file in $requiredFiles) {
    Add-Check $checks "required_file:$file" (Test-PathNonEmpty $file) $file
}

$auditPath = "data/processed/report_tables/submission_audit_summary.csv"
if (Test-Path -LiteralPath $auditPath) {
    $auditRows = Import-Csv -LiteralPath $auditPath
    $problemCount = ($auditRows | Measure-Object -Property problems -Sum).Sum
    Add-Check $checks "submission_audit_zero_problems" ($problemCount -eq 0) "problem_count=$problemCount"
}
else {
    Add-Check $checks "submission_audit_zero_problems" $false "audit summary missing"
}

$checks | Format-Table -AutoSize

$failed = @($checks | Where-Object { -not $_.passed })
if ($failed.Count -gt 0) {
    Write-Host "`nFailed checks:" -ForegroundColor Red
    $failed | Format-Table -AutoSize
    exit 1
}

Write-Host "`nAll final submission checks passed." -ForegroundColor Green
