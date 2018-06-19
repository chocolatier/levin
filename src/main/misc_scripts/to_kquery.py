# USAGE: python to_kquery.py path/to/kq/files size_of_symbolic_input location_of_allqueries

import os
import sys

conversion_dir = sys.argv[1]
size = sys.argv[2]

def getFiles(path_list):
    return [p for p in path_list if os.path.isfile(os.path.join(conversion_dir, p))]

def to_kquery(string, size, const_array = []):
    if const_array == []:
        return "array v0___symfile____tmp_input___0_1_symfile___0["  + str(size) + "] : w32 -> w8 = symbolic\n(query [" + string + "]\nfalse []\n[v0___symfile____tmp_input___0_1_symfile___0])\n"
    else:
        return "array v0___symfile____tmp_input___0_1_symfile___0["  + str(size) + "] : w32 -> w8 = symbolic\n" + "\n".join(const_array) + "\n(query [" + string + "]\nfalse []\n[v0___symfile____tmp_input___0_1_symfile___0])\n"

def grab_const_arrays(fileloc):
    x = filter(lambda l: "array const_arr" in l,open(fileloc).readlines())
    print(set(x))
    return set(x)

for f in getFiles(os.listdir(conversion_dir)):
    x = open(os.path.join(conversion_dir,f)).read()
    if (len(sys.argv) == 4):
        y = grab_const_arrays(sys.argv[3])
    else:
        y = []
    new_str = to_kquery(x, size, y)
    g = open(os.path.join(conversion_dir,str(f)[:-3] + ".kquery"), "w")
    g.write(new_str)
    g.close()
