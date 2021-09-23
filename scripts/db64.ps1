$pathSrc = $args[0]
$pathTgt = $args[1]

Write-Host "Dencoding " $pathSrc "to" $pathTgt
$encoding = [System.Text.Encoding]::ASCII
$B64String = [System.IO.File]::ReadAllText($pathSrc, $encoding)
$bytes = [System.Convert]::FromBase64String($B64String)
[System.IO.File]::WriteAllBytes($pathTgt, $bytes)
