-- Terminal needs to check for "Connected" text before sending data

-- then use sk:send("mydata") to send data to server, the callback func should print whats received on serial port

sk = net.createConnection(net.TCP, 0)

sk:on("receive",  function(sk, c) print(c) end )
sk:on("connection",  function() print("Connected") end )

tmr.alarm(1,1000, 1,
  function()
    if wifi.sta.getip()~=nil then
      tmr.stop(1)
      print("\r\nOK " .. wifi.sta.getip() .. " in " .. tmr.now()/1000 .. " mS")
      if file.list()["host.cfg"] then 
        file.open("host.cfg", "r")
 	    server = file.readline():gsub("%s+$", "")
        file.close()
        sk:connect(2001,server)
	  end
    elseif tmr.time()>10 then
      tmr.stop(1)
      print("\r\nFail!")
    end
  end
)

-- both SSID and Password are CaSe SeNsITiVe! and must be enclosed in quotes! eg SetWIFI("Indevin","Password123","192.168.0.2")
function SetWIFI(SSID,pwd,host)
  wifi.setmode(wifi.STATION)
  wifi.sta.config(SSID,pwd)
  wifi.sta.autoconnect(1)  
  file.remove("host.cfg")
  file.open("host.cfg", "w+")
  file.writeline(host)
  file.close()
  print("\r\nOK")
end

