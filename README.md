# sd-generate

This repository aims to implement the formalization, automatic generation, and rendering of UML state diagrams.

It currently contains the following functionalities:
 - Under the folder `alloy`, it provides the formalization of UML state diagrams in Alloy. User can run `uml_state_diagram.als` to generate different Alloy sd instances and play around. 
 - Under the folder `app`, it provides the following executables:
	- `draw.hs`: draws Alloy sd instances via Haskell `diagrams` library.
	- `drawing.hs`: draws Haskell sd instances via `diagrams` library.
	- `enumeratedArrowsDirectDemo.hs`: generates a demo of sd "flatten" tasks, but currently not supporting concurrent states and history nodes.
	- `enumeratedArrowsQuizDemo.hs`: a quiz task corresponding to the "flatten" demo.
	- `sampling.hs`: generates random Haskell sd instances and draws them via `diagrams` library.
	- `transformAlloy.hs`: transforms Haskell sd instances into Alloy code that describes exactly those instances.
	- `transformPlantUML.hs`: transforms Haskell sd instances into plantUML code and then renders them. There are some constraints that can be found in [plantUML/document.txt](https://github.com/fmidue/sd-generate/blob/main/plantUML/document.txt) and [issue 25](https://github.com/fmidue/sd-generate/issues/25)

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
```
