BEGIN { FS="\t" }
{
    res += ($2 == 0) ? 1 : 0
}
END {
    print res
}
