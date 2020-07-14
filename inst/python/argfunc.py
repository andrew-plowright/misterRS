
def niceBool(v):
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

def niceInt(v):
    if v.lower() in ('', 'nan', 'na'):
        return None
    else:
        return int(v)

def niceFloat(v):
    if v.lower() in ('', 'nan', 'na'):
        return None
    else:
        return float(v)

def niceStr(v):
    if v.lower() in ('', 'nan', 'na'):
        return None
    else:
        return str(v)