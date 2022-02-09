module diagram // name: "", (irrelevant) label: 86
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends NormalStates{}{
  name = Name1 // "0"
}
one sig N_2 extends NormalStates{}{
  name = Name2 // "1"
}
one sig N_3 extends NormalStates{}{
  name = Name3 // "2"
}
one sig N_4 extends NormalStates{}{
  name = Name4 // "3"
}
one sig N_5 extends NormalStates{}{
  name = Name5 // "4"
}
one sig N_6 extends NormalStates{}{
  name = Name6 // "5"
}
one sig N_7 extends NormalStates{}{
  name = Name7 // "6"
}
one sig N_8 extends NormalStates{}{
  name = Name8 // "7"
}
one sig N_9 extends NormalStates{}{
  name = Name9 // "8"
}
one sig N_10 extends NormalStates{}{
  name = Name10 // "9"
}
one sig N_11 extends NormalStates{}{
  name = Name11 // "10"
}
one sig N_12 extends NormalStates{}{
  name = Name12 // "11"
}
one sig N_13 extends NormalStates{}{
  name = Name13 // "12"
}
one sig N_14 extends NormalStates{}{
  name = Name14 // "13"
}
one sig N_15 extends NormalStates{}{
  name = Name15 // "14"
}
one sig N_16 extends NormalStates{}{
  name = Name16 // "15"
}
one sig N_17 extends NormalStates{}{
  name = Name17 // "16"
}
one sig N_18 extends NormalStates{}{
  name = Name18 // "17"
}
one sig N_19 extends NormalStates{}{
  name = Name19 // "18"
}
one sig N_20 extends NormalStates{}{
  name = Name20 // "19"
}
one sig N_21 extends NormalStates{}{
  name = Name21 // "20"
}
one sig N_22 extends NormalStates{}{
  name = Name22 // "21"
}
one sig N_23 extends NormalStates{}{
  name = Name23 // "22"
}
one sig N_24 extends NormalStates{}{
  name = Name24 // "23"
}
one sig N_25 extends NormalStates{}{
  name = Name25 // "24"
}
one sig N_26 extends NormalStates{}{
  name = Name26 // "25"
}
one sig N_27 extends NormalStates{}{
  name = Name27 // "26"
}
one sig N_28 extends NormalStates{}{
  name = Name28 // "27"
}
one sig N_29 extends NormalStates{}{
  name = Name29 // "28"
}
one sig N_30 extends NormalStates{}{
  name = Name30 // "29"
}
one sig N_31 extends NormalStates{}{
  name = Name31 // "30"
}
one sig N_32 extends NormalStates{}{
  name = Name32 // "31"
}
one sig N_33 extends NormalStates{}{
  name = Name33 // "32"
}
one sig N_34 extends NormalStates{}{
  name = Name34 // "33"
}
one sig N_35 extends NormalStates{}{
  name = Name35 // "34"
}
one sig N_36 extends NormalStates{}{
  name = Name36 // "35"
}
one sig N_37 extends NormalStates{}{
  name = Name37 // "36"
}
one sig N_38 extends NormalStates{}{
  name = Name38 // "37"
}
one sig N_39 extends NormalStates{}{
  name = Name39 // "38"
}
one sig N_40 extends NormalStates{}{
  name = Name40 // "39"
}
one sig N_41 extends NormalStates{}{
  name = Name41 // "40"
}
one sig N_42 extends NormalStates{}{
  name = Name42 // "41"
}
one sig N_43 extends NormalStates{}{
  name = Name43 // "42"
}
one sig N_44 extends NormalStates{}{
  name = Name44 // "43"
}
one sig N_45 extends NormalStates{}{
  name = Name45 // "44"
}
one sig N_46 extends NormalStates{}{
  name = Name46 // "45"
}
one sig N_47 extends NormalStates{}{
  name = Name47 // "46"
}
one sig N_48 extends NormalStates{}{
  name = Name48 // "47"
}
one sig N_49 extends NormalStates{}{
  name = Name49 // "48"
}
one sig N_50 extends NormalStates{}{
  name = Name50 // "49"
}
one sig N_51 extends NormalStates{}{
  name = Name51 // "50"
}
one sig N_52 extends NormalStates{}{
  name = Name52 // "51"
}
one sig N_53 extends NormalStates{}{
  name = Name53 // "52"
}
one sig N_54 extends NormalStates{}{
  name = Name54 // "53"
}
one sig N_55 extends NormalStates{}{
  name = Name55 // "54"
}
one sig N_56 extends NormalStates{}{
  name = Name56 // "55"
}
one sig N_57 extends NormalStates{}{
  name = Name57 // "56"
}
one sig N_58 extends NormalStates{}{
  name = Name58 // "57"
}
one sig N_59 extends NormalStates{}{
  name = Name59 // "58"
}
one sig N_60 extends NormalStates{}{
  name = Name60 // "59"
}

one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "after(1min)"
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_2
  label = T1 // "after(1min)"
  to = N_3
}
one sig Connection3 extends Flows{}{
  from = N_3
  label = T1 // "after(1min)"
  to = N_4
}
one sig Connection4 extends Flows{}{
  from = N_4
  label = T1 // "after(1min)"
  to = N_5
}
one sig Connection5 extends Flows{}{
  from = N_5
  label = T1 // "after(1min)"
  to = N_6
}
one sig Connection6 extends Flows{}{
  from = N_6
  label = T1 // "after(1min)"
  to = N_7
}
one sig Connection7 extends Flows{}{
  from = N_7
  label = T1 // "after(1min)"
  to = N_8
}
one sig Connection8 extends Flows{}{
  from = N_8
  label = T1 // "after(1min)"
  to = N_9
}
one sig Connection9 extends Flows{}{
  from = N_9
  label = T1 // "after(1min)"
  to = N_10
}
one sig Connection10 extends Flows{}{
  from = N_10
  label = T1 // "after(1min)"
  to = N_11
}
one sig Connection11 extends Flows{}{
  from = N_11
  label = T1 // "after(1min)"
  to = N_12
}
one sig Connection12 extends Flows{}{
  from = N_12
  label = T1 // "after(1min)"
  to = N_13
}
one sig Connection13 extends Flows{}{
  from = N_13
  label = T1 // "after(1min)"
  to = N_14
}
one sig Connection14 extends Flows{}{
  from = N_14
  label = T1 // "after(1min)"
  to = N_15
}
one sig Connection15 extends Flows{}{
  from = N_15
  label = T1 // "after(1min)"
  to = N_16
}
one sig Connection16 extends Flows{}{
  from = N_16
  label = T1 // "after(1min)"
  to = N_17
}
one sig Connection17 extends Flows{}{
  from = N_17
  label = T1 // "after(1min)"
  to = N_18
}
one sig Connection18 extends Flows{}{
  from = N_18
  label = T1 // "after(1min)"
  to = N_19
}
one sig Connection19 extends Flows{}{
  from = N_19
  label = T1 // "after(1min)"
  to = N_20
}
one sig Connection20 extends Flows{}{
  from = N_20
  label = T1 // "after(1min)"
  to = N_21
}
one sig Connection21 extends Flows{}{
  from = N_21
  label = T1 // "after(1min)"
  to = N_22
}
one sig Connection22 extends Flows{}{
  from = N_22
  label = T1 // "after(1min)"
  to = N_23
}
one sig Connection23 extends Flows{}{
  from = N_23
  label = T1 // "after(1min)"
  to = N_24
}
one sig Connection24 extends Flows{}{
  from = N_24
  label = T1 // "after(1min)"
  to = N_25
}
one sig Connection25 extends Flows{}{
  from = N_25
  label = T1 // "after(1min)"
  to = N_26
}
one sig Connection26 extends Flows{}{
  from = N_26
  label = T1 // "after(1min)"
  to = N_27
}
one sig Connection27 extends Flows{}{
  from = N_27
  label = T1 // "after(1min)"
  to = N_28
}
one sig Connection28 extends Flows{}{
  from = N_28
  label = T1 // "after(1min)"
  to = N_29
}
one sig Connection29 extends Flows{}{
  from = N_29
  label = T1 // "after(1min)"
  to = N_30
}
one sig Connection30 extends Flows{}{
  from = N_30
  label = T1 // "after(1min)"
  to = N_31
}
one sig Connection31 extends Flows{}{
  from = N_31
  label = T1 // "after(1min)"
  to = N_32
}
one sig Connection32 extends Flows{}{
  from = N_32
  label = T1 // "after(1min)"
  to = N_33
}
one sig Connection33 extends Flows{}{
  from = N_33
  label = T1 // "after(1min)"
  to = N_34
}
one sig Connection34 extends Flows{}{
  from = N_34
  label = T1 // "after(1min)"
  to = N_35
}
one sig Connection35 extends Flows{}{
  from = N_35
  label = T1 // "after(1min)"
  to = N_36
}
one sig Connection36 extends Flows{}{
  from = N_36
  label = T1 // "after(1min)"
  to = N_37
}
one sig Connection37 extends Flows{}{
  from = N_37
  label = T1 // "after(1min)"
  to = N_38
}
one sig Connection38 extends Flows{}{
  from = N_38
  label = T1 // "after(1min)"
  to = N_39
}
one sig Connection39 extends Flows{}{
  from = N_39
  label = T1 // "after(1min)"
  to = N_40
}
one sig Connection40 extends Flows{}{
  from = N_40
  label = T1 // "after(1min)"
  to = N_41
}
one sig Connection41 extends Flows{}{
  from = N_41
  label = T1 // "after(1min)"
  to = N_42
}
one sig Connection42 extends Flows{}{
  from = N_42
  label = T1 // "after(1min)"
  to = N_43
}
one sig Connection43 extends Flows{}{
  from = N_43
  label = T1 // "after(1min)"
  to = N_44
}
one sig Connection44 extends Flows{}{
  from = N_44
  label = T1 // "after(1min)"
  to = N_45
}
one sig Connection45 extends Flows{}{
  from = N_45
  label = T1 // "after(1min)"
  to = N_46
}
one sig Connection46 extends Flows{}{
  from = N_46
  label = T1 // "after(1min)"
  to = N_47
}
one sig Connection47 extends Flows{}{
  from = N_47
  label = T1 // "after(1min)"
  to = N_48
}
one sig Connection48 extends Flows{}{
  from = N_48
  label = T1 // "after(1min)"
  to = N_49
}
one sig Connection49 extends Flows{}{
  from = N_49
  label = T1 // "after(1min)"
  to = N_50
}
one sig Connection50 extends Flows{}{
  from = N_50
  label = T1 // "after(1min)"
  to = N_51
}
one sig Connection51 extends Flows{}{
  from = N_51
  label = T1 // "after(1min)"
  to = N_52
}
one sig Connection52 extends Flows{}{
  from = N_52
  label = T1 // "after(1min)"
  to = N_53
}
one sig Connection53 extends Flows{}{
  from = N_53
  label = T1 // "after(1min)"
  to = N_54
}
one sig Connection54 extends Flows{}{
  from = N_54
  label = T1 // "after(1min)"
  to = N_55
}
one sig Connection55 extends Flows{}{
  from = N_55
  label = T1 // "after(1min)"
  to = N_56
}
one sig Connection56 extends Flows{}{
  from = N_56
  label = T1 // "after(1min)"
  to = N_57
}
one sig Connection57 extends Flows{}{
  from = N_57
  label = T1 // "after(1min)"
  to = N_58
}
one sig Connection58 extends Flows{}{
  from = N_58
  label = T1 // "after(1min)"
  to = N_59
}
one sig Connection59 extends Flows{}{
  from = N_59
  label = T1 // "after(1min)"
  to = N_60
}
one sig Connection60 extends Flows{}{
  from = N_60
  label = T1 // "after(1min)"
  to = N_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
one sig Name8 extends ComponentNames{}
one sig Name9 extends ComponentNames{}
one sig Name10 extends ComponentNames{}
one sig Name11 extends ComponentNames{}
one sig Name12 extends ComponentNames{}
one sig Name13 extends ComponentNames{}
one sig Name14 extends ComponentNames{}
one sig Name15 extends ComponentNames{}
one sig Name16 extends ComponentNames{}
one sig Name17 extends ComponentNames{}
one sig Name18 extends ComponentNames{}
one sig Name19 extends ComponentNames{}
one sig Name20 extends ComponentNames{}
one sig Name21 extends ComponentNames{}
one sig Name22 extends ComponentNames{}
one sig Name23 extends ComponentNames{}
one sig Name24 extends ComponentNames{}
one sig Name25 extends ComponentNames{}
one sig Name26 extends ComponentNames{}
one sig Name27 extends ComponentNames{}
one sig Name28 extends ComponentNames{}
one sig Name29 extends ComponentNames{}
one sig Name30 extends ComponentNames{}
one sig Name31 extends ComponentNames{}
one sig Name32 extends ComponentNames{}
one sig Name33 extends ComponentNames{}
one sig Name34 extends ComponentNames{}
one sig Name35 extends ComponentNames{}
one sig Name36 extends ComponentNames{}
one sig Name37 extends ComponentNames{}
one sig Name38 extends ComponentNames{}
one sig Name39 extends ComponentNames{}
one sig Name40 extends ComponentNames{}
one sig Name41 extends ComponentNames{}
one sig Name42 extends ComponentNames{}
one sig Name43 extends ComponentNames{}
one sig Name44 extends ComponentNames{}
one sig Name45 extends ComponentNames{}
one sig Name46 extends ComponentNames{}
one sig Name47 extends ComponentNames{}
one sig Name48 extends ComponentNames{}
one sig Name49 extends ComponentNames{}
one sig Name50 extends ComponentNames{}
one sig Name51 extends ComponentNames{}
one sig Name52 extends ComponentNames{}
one sig Name53 extends ComponentNames{}
one sig Name54 extends ComponentNames{}
one sig Name55 extends ComponentNames{}
one sig Name56 extends ComponentNames{}
one sig Name57 extends ComponentNames{}
one sig Name58 extends ComponentNames{}
one sig Name59 extends ComponentNames{}
one sig Name60 extends ComponentNames{}

one sig T1 extends TriggerNames{}

fact{
  // Flows = SFlow + Connection1 + Connection2 + Connection3 + Connection4 + Connection5 + Connection6 + Connection7 + Connection8 + Connection9 + Connection10 + Connection11 + Connection12 + Connection13 + Connection14 + Connection15 + Connection16 + Connection17 + Connection18 + Connection19 + Connection20 + Connection21 + Connection22 + Connection23 + Connection24 + Connection25 + Connection26 + Connection27 + Connection28 + Connection29 + Connection30 + Connection31 + Connection32 + Connection33 + Connection34 + Connection35 + Connection36 + Connection37 + Connection38 + Connection39 + Connection40 + Connection41 + Connection42 + Connection43 + Connection44 + Connection45 + Connection46 + Connection47 + Connection48 + Connection49 + Connection50 + Connection51 + Connection52 + Connection53 + Connection54 + Connection55 + Connection56 + Connection57 + Connection58 + Connection59 + Connection60
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // no HierarchicalStates
  // no RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 EndNodes, 0 HierarchicalStates, 0 RegionsStates, 0 Regions, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 86 ProtoFlows, exactly 61 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
