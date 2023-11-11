StateDiagram {
  substates = [
    StateDiagram {
      substates = [
        StateDiagram {
          substates = [
            Fork {label = 1},
            CombineDiagram {
              substates = [
                StateDiagram {
                  substates = [
                    InnerMostState {label = 1, name = "1", operations = ""},
                    InnerMostState {label = 2, name = "2", operations = ""}
                    ],
                  label = 1,
                  name = "C",
                  connections = [
                    Connection {pointFrom = [1], pointTo = [2], transition = "a"},
                    Connection {pointFrom = [2], pointTo = [1], transition = "a"}
                    ],
                  startState = [1]
                  },
                StateDiagram {
                  substates = [
                    InnerMostState {label = 1, name = "3", operations = ""},
                    InnerMostState {label = 2, name = "4", operations = ""},
                    InnerMostState {label = 3, name = "5", operations = ""}
                    ],
                  label = 2,
                  name = "",
                  connections = [
                    Connection {pointFrom = [1], pointTo = [2], transition = "b"},
                    Connection {pointFrom = [2], pointTo = [3], transition = "b"},
                    Connection {pointFrom = [3], pointTo = [1], transition = "b"}
                    ],
                  startState = [1]
                  }
                ],
              label = 2
              },
            Join {label = 3},
            History {label = 4, historyType = Shallow},
            StateDiagram {
              substates = [
                InnerMostState {label = 1, name = "9", operations = ""},
                InnerMostState {label = 2, name = "10", operations = ""}
                ],
              label = 5,
              name = "D",
              connections = [
                Connection {pointFrom = [1], pointTo = [2], transition = "g"},
                Connection {pointFrom = [2], pointTo = [1], transition = "g"}
                ],
              startState = [1]
              }
            ],
          label = 1,
          name = "B",
          connections = [
            Connection {pointFrom = [4], pointTo = [5], transition = ""},
            Connection {pointFrom = [1], pointTo = [2,1,1], transition = ""},
            Connection {pointFrom = [1], pointTo = [2,2,3], transition = ""},
            Connection {pointFrom = [2,1,2], pointTo = [3], transition = "c"},
            Connection {pointFrom = [2,2,2], pointTo = [3], transition = "c"}
            ],
          startState = []
          },
        StateDiagram {
          substates = [
            InnerMostState {label = 1, name = "6", operations = ""},
            InnerMostState {label = 2, name = "7", operations = ""},
            InnerMostState {label = 3, name = "8", operations = ""}
            ],
          label = 2,
          name = "E",
          connections = [
            Connection {pointFrom = [1], pointTo = [3], transition = "d"},
            Connection {pointFrom = [3], pointTo = [2], transition = "d"},
            Connection {pointFrom = [2], pointTo = [1], transition = "d"}
            ],
          startState = [3]
          }
        ],
      label = 1,
      name = "A",
      connections = [
        Connection {pointFrom = [1,2,1,1], pointTo = [2], transition = "i"},
        Connection {pointFrom = [1,3], pointTo = [2], transition = ""},
        Connection {pointFrom = [2,2], pointTo = [1,2,2,3], transition = "e"},
        Connection {pointFrom = [1,5,2], pointTo = [2,2], transition = "h"},
        Connection {pointFrom = [2,1], pointTo = [1,4], transition = "f"}
        ],
      startState = []
      }
    ],
  label = 35,
  name = "",
  connections = [],
  startState = [1,1,1]
}
