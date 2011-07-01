all: BargainServer

BargainServer: BargainServer.hs */*.hs
	ghc -o BargainServer $< --make -O2

AgentVsAgent: AgentVsAgent.hs
	ghc -o AgentVsAgent $< --make -O2

clean:
	rm -f *.hi *.o */*.hi */*.o BargainServer AgentVsAgent
