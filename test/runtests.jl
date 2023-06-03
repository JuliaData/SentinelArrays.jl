susing ReTest

using SentinelArrays

include("SentinelArrayTests.jl")

retest(SentinelArrays, SentinelArrayTests)
