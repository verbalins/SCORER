import math
import pandas

def FPM(minimumSig, parameterNames, selectedData, unselectedData, useEquality = True, onlyMostSignificant = True):
    """
    minimumSig: minsta tillåtna värdet (0.5)
    parameterNames: en vanlig lista med variabelnamnen
    selectedData: en matris (lista med listor) av variabelvärden
    unselectedData: en matris (lista med listor) av variabelvärden
    useEquality: ifall du vill kolla ==
    onlyMostSignificant: med true blir det som matlab versionen, annars tar den med alla regler
    """
    if isinstance(selectedData, pandas.DataFrame):
        selectedData = selectedData.values
    
    if isinstance(unselectedData, pandas.DataFrame):
        unselectedData = unselectedData.values

    if len(selectedData) == 0:
        raise Exception("selected data was empty")
    elif len(unselectedData) == 0:
        raise Exception("unselected data was empty")
        
    numVariables = len(selectedData[0])
    
    selMembers = [MakeMember(x, True) for x in selectedData]
    unselMembers = [MakeMember(x, False) for x in unselectedData]
    
    dataMembers = []
    for x in selMembers:
        dataMembers.append(x)
    for x in unselMembers:
        dataMembers.append(x)
        
    levelmat = [[] for x in range(numVariables)]
    totalDistinctVars = 0
    
    for i in range(numVariables):
        data = [x.variables[i] for x in dataMembers]
        data = list(set(data))
        levelmat[i] = data
        totalDistinctVars = totalDistinctVars + len(levelmat[i])
        
    ueq = 3
    if not useEquality:
        ueq = 2
        
    noofcolumns = totalDistinctVars * ueq - (2*numVariables)
    
    columns = [None] * noofcolumns
    colIndex = 0
    
    for varI in range(numVariables):
        paramName = parameterNames[varI]
        levelS = len(levelmat[varI])
        for levelI in range(levelS):
            if levelI != 0:
                columns[colIndex] = MakeColumn(lambda x1, x2: x1 < x2, '<', dataMembers, levelmat, paramName, 0, varI, levelI)
                colIndex = colIndex+1
            if useEquality:
                columns[colIndex] = MakeColumn(lambda x1, x2: x1 == x2, '==', dataMembers, levelmat, paramName, 1, varI, levelI)
                colIndex = colIndex+1
            if levelI != levelS-1:
                ueq = 2
                if not useEquality:
                    ueq = 1
                columns[colIndex] = MakeColumn(lambda x1, x2: x1 > x2, '>', dataMembers, levelmat, paramName, ueq, varI, levelI)
                colIndex = colIndex+1
                
    mdrs = [None] * noofcolumns
    for i in range(noofcolumns):
        mdrs[i] = MakeMdr(columns[i], minimumSig*100.0, len(selMembers), len(unselMembers))
        
    mdrs.sort(reverse=True, key=lambda x: x.Value)
    
    firstZero = 0
    while firstZero < len(mdrs) and mdrs[firstZero].Value != 0.0:
        firstZero = firstZero + 1
        
    if useEquality:
        consideredVars = [[False]*numVariables for x in range(3)]
    else:
        consideredVars = [[False]*numVariables for x in range(2)]
        
    finalMDRs = [None] * len(consideredVars) * numVariables
    numAdded = 0
    
    i = 0
    while i < firstZero and numAdded < len(finalMDRs):
        column = mdrs[i].Column
        if consideredVars[column.OperatorIndex][column.VariableIndex] == False:
            consideredVars[column.OperatorIndex][column.VariableIndex] = True
            finalMDRs[numAdded] = mdrs[i]
            numAdded = numAdded+1
            
        i = i+1
    if not onlyMostSignificant:
        finalMDRs = [None]*firstZero
        numAdded = 0

        for x in range(firstZero):
            finalMDRs[x] = mdrs[x]
            numAdded = numAdded+1
            
    if useEquality:
        finalFinalMDRs = [None]*numAdded
        added = 0
        for i in range(numAdded):
            save = True
            m = finalMDRs[i]
            if m.Column.OperatorIndex != 1:
                for j in range(numAdded):
                    m2 = finalMDRs[j]
                    if m2.Column.OperatorIndex == 1 and m2.Value == m.Value:
                        save = False
            if save:
                finalFinalMDRs[added] = m
                added = added+1
                
        finalMDRs = finalFinalMDRs
        numAdded = added
        
    rules = []
    for i in range(numAdded):
        mdr = finalMDRs[i]
        r = lambda: None
        r.ParameterName = mdr.Column.GeneratedRule.ParameterName
        r.Sign = mdr.Column.GeneratedRule.Sign
        r.Significance = mdr.Significance / 100.0
        r.UnselectedSignificance = mdr.UnSignificance / 100.0
        r.Value = mdr.Column.GeneratedRule.Value
        r.Ratio = mdr.Value
        
        if r.Significance >= minimumSig:
            rules.append(r)
    
    return rules
    
def MakeMember(variables, selected):
    vars = [math.ceil(x * 1000.0)/1000.0 for x in variables]
    sel = selected
    
    m = lambda: None
    m.variables = vars
    m.selected = sel
    return m
    
def MakeColumn(comp, sign, dataMembers, levelMatrix, paramName, operatorIndex, variableIndex, levelIndex):
    TruePositives = 0
    FalsePositives = 0

    for member in dataMembers:
        positive = comp(member.variables[variableIndex], levelMatrix[variableIndex][levelIndex])
        if positive and member.selected:
            TruePositives = TruePositives+1
        if positive and not member.selected:
            FalsePositives = FalsePositives+1
    
    GeneratedRule = MakeRule(paramName, sign, levelMatrix[variableIndex][levelIndex])
    
    c = lambda: None
    c.TruePositives = TruePositives
    c.FalsePositives = FalsePositives
    c.GeneratedRule = GeneratedRule
    c.VariableIndex = variableIndex
    c.LevelIndex = levelIndex
    c.OperatorIndex = operatorIndex
    
    return c
    
def MakeRule(paramName, sign, value):
    r = lambda: None
    r.ParameterName = paramName
    r.Sign = sign
    r.Value = value
    
    return r
    
def MakeMdr(column, minSig, noSelMembers, noUnselMembers):
    ratio = column.TruePositives / (column.FalsePositives + 1)
    
    mdr = lambda: None 
    mdr.Column = column
    mdr.Significance = (column.TruePositives * 100.0) / noSelMembers
    mdr.UnSignificance = (column.FalsePositives * 100.0) / noUnselMembers
    
    mdr.Value = 0.0
    if mdr.Significance >= minSig:
        mdr.Value = ratio
    
    return mdr

def ExportRules(rules):
    df_rules = pandas.DataFrame(columns=['Parameter', 'Sign', 'Value', 'SEL', 'UNSEL', 'Ratio'])
    for rule in rules:
        df_rules.loc[len(df_rules)] = [rule.ParameterName, rule.Sign, rule.Value, rule.Significance, rule.UnselectedSignificance, rule.Ratio]
    return(df_rules)
