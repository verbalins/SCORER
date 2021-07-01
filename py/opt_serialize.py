import py.opt_pb2 as Opt

def ParseOutput(rawdata):
    t = Opt.Output()
    return(t.FromString(rawdata))

def ParseOutputReplication(rawdata):
    t = Opt.OutputReplication()
    return(t.FromString(rawdata))

def ParseInput(rawdata):
    t = Opt.Input()
    return(t.FromString(rawdata))

def ParseExperiment(rawdata):
    t = Opt.OutputReplications()
    return(t.FromString(rawdata))
