import socket
import os

s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
s.bind(('',5555))
s.listen(1)
conn,addr = s.accept()
print 'Connected by',addr
toplevel = ""

##todo: write into file and compile!
id = 0
def repl(cc):
    while 1:
        data = cc.recv(10000)
        if data=="": break
        if not data: break
        try:
            file_name = "/tmp/"+str(id)
            if (os.path.exists(file_name)):
                os.remove(file_name)
            f = open(file_name,'w')
            f.write(data)
            print eval(compile(f))    
        except Exception,e:
            print str(e)
       
try:
    repl(conn)
except Exception,e:
    conn.close()
    print str(e)
