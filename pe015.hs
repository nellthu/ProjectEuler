-- choose 20 positions of "go south" command in time from 40 total commands composed of "go south" or "go east" to get from one corner to the other

main = product [21..40] `div` product [1..20]

