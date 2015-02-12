$mainClass = "com.how_hard_can_it_be.primes.App"

$classPathElements=$(cat classpath.txt | ss -NotMatch '^[ 	]*#|^[ 	]*$')
write-debug "classPathElements = $classPathElements"
$classPath=""
foreach ($elt in $classPathElements)
{
	$elt = $elt.ToString().Replace( "~", $env:USERPROFILE)
    if ($(Test-Path $elt))
	{
        if ( "$classPath" -eq "" ) {}
        else
		{
            $classPath="$classPath;"
		}
        $classPath="$classPath$elt"
	}
    else
	{
        Write-Warning ("File not readable: {0}" -f $elt)
    }
}

java -Xmx3G -cp $classPath $mainClass $Args
