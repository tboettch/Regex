
perf: regex.hs perfTest.hs
	ghc -O2 --make perfTest.hs -prof -auto-all -rtsopts -fforce-recomp -o perfTest.exe
	perfTest.exe 1000 +RTS -M1G -hy
	hp2ps -c perfTest.hp

clean:
	rm *.exe
	rm *.hi
	rm *.o
	rm *.aux
	rm *.hp
	rm *.ps
