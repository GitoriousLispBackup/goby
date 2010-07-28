import socket
import os

import sys
import traceback
import linecache

def get_traceback():
    et, ev, tb = sys.exc_info()
    line_number = None
    file_name = None
    while tb :                                                                  
        co = tb.tb_frame.f_code                                                 
        filename =  str(co.co_filename)
        lineno = traceback.tb_lineno(tb)      
        if filename=="/tmp/tmp1.py":
            line_number = lineno
            file_name = filename                                                 
        tb = tb.tb_next                                                         
    return line_number,filename

def handle_traceback(e):
    line,file = get_traceback()
    if line!=None and file!=None:
        print "\nTRACEBACK CODE:"
        for i in range(1,line+1):
            print linecache.getline(file,i).strip()
        linecache.clearcache()
        print "END TRACEBACK\n"
    traceback.print_exc()
  




s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
s.bind(('',5556))
s.listen(1)
conn,addr = s.accept()
print 'Connected by',addr
toplevel = ""

##todo: write into file and compile!
id = 0
def repl(cc):
    while 1:
        data = cc.recv(100000000)
        if data=="": break
        if not data: break

        tmp_file = "/tmp/tmp1.py"
        if os.path.exists(tmp_file):
            os.remove(tmp_file)
        f = open(tmp_file,'w')
        f.write(data)
        f.close()
        try:
            eval(compile(data,tmp_file,'exec'))
        except Exception,e:
            handle_traceback(e)
       
try:
    repl(conn)
except Exception,e:
    conn.close()
    print "error, closing connection"
    print str(e)
conn.close()
