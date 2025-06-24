# sd-generate
This repository currently contains the following functions:
 - Under the folder ./alloy, it provides the formalization of UML state diagrams in Alloy. User can use `uml_state_diagram.als` to generate different sd instances and play around. 
 - Under the folder ./app, it provides 8 functions:
	- `draw.hs`: draws Alloy sd instances with our Haskell visualization tool.
	- `drawing.hs`: draws Haskell sd instances.
	- `enumeratedArrowsDirectDemo.hs`: generates a demo of sd "flatten" programs, but currently not support concurrent states and history nodes.
	- `enumeratedArrowsQuizDemo.hs`: a quiz task corresponds to the "flatten" demo.
	- `sampling.hs`: generates and draws random Haskell sd instances.
	- `transformAlloy.hs`: transforms Haskell sd instances into Alloy sd instances.
	- `transformPlantUML.hs`: transforms Haskell sd instances into plantUML texts. There are some constraints that can be found in [https://github.com/fmidue/sd-generate/blob/main/plantUML/document.txt](https://github.com/fmidue/sd-generate/blob/main/plantUML/document.txt) and [issues 25](https://github.com/fmidue/sd-generate/issues/25)

# Requirements
- Alloy Analyzer
- Haskell GHC & Stack
- Java JDK/JRE (> 8)
- Graphviz
- PlantUML

# How to Run?
```bash
stack build
stack exec <app name e.g. transformAlloy>