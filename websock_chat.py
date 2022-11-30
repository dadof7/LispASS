# -*- coding: utf-8 -*-

from SimpleWebSocketServer import SimpleWebSocketServer, WebSocket
import datetime

Port = 5003

clients = []
welcomeMessage = ";;; *** welcome to" # これが1chunkで送られる保証は無いが...とりあえず

class SimpleChat(WebSocket):

    def handleMessage(self):
        self.messageCount += 1
        
        if self.messageCount == 1 and welcomeMessage in self.data: # lispになろうとしている
            for client in clients:
                if client.IamLisp:
                    print(self.address, "*** other lisp working already => closing ***")
                    self.sendClose() # 既にlispが居る, 1つのlispしか許さない
                    return
            self.IamLisp = True
            print(self.address, "*** recognized as a lisp interpreter ***")

        # もし自分がlispだったら、自分以外のクライアント全てに送る.
        # だれかが評価した式の結果なので.
        # もし自分がクライアントなら、lispにだけ送る.
        # そうしないと他の人の画面が乱れる.
        if self.IamLisp:
            for client in clients:
                if client != self: 
                    client.sendMessage(self.data)
        else:
            for client in clients:
                if client.IamLisp:
                    client.sendMessage(self.data)

    def handleConnected(self):
        print(self.address, 'connected', datetime.datetime.now())
        self.IamLisp = False
        self.messageCount = 0
        clients.append(self)

    def handleClose(self):
        clients.remove(self)
        print(self.address, 'closed', datetime.datetime.now())

server = SimpleWebSocketServer('', Port, SimpleChat)
server.serveforever()
