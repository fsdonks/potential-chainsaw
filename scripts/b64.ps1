$pathSrc = $args[0]
$pathTgt = $args[1]
Write-Host "Encoding " $pathSrc "to" $pathTgt

$encoding = [System.Text.Encoding]::ASCII
$bytes = [System.IO.File]::ReadAllBytes($pathSrc)
$B64String = [System.Convert]::ToBase64String($bytes, [System.Base64FormattingOptions]::None)
[System.IO.File]::WriteAllText($pathTgt, $B64String, $encoding)
