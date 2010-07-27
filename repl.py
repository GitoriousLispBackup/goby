import socket
s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
s.bind(('',5555))
s.listen(1)
conn,addr = s.accept()
print 'Connected by',addr
toplevel = ""

def repl(cc):
    while 1:
        data = cc.recv(10000)
        if data=="": break
        if not data: break
        try:
            exec(data)
            print toplevel
            toplevel = ""
        except Exception,e:
            print str(e)
       
try:
    repl(conn)
except Exception,e:
    conn.close()
    print str(e)
