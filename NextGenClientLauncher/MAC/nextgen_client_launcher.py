#!/usr/bin/env python
import wx
import time
import os.path
import urllib2
import zipfile
import platform 
import traceback
import subprocess
import xml.etree.ElementTree as ET

SERVER_XML_URL = "http://www.abydosentertainment.com/WorldRecordClientXML"
CLIENT_DOWNLOAD_URL = "http://www.abydosentertainment.com/WorldRecordClient"
CLIENT_ZIP_FILE = "WorldRecordClient.zip"
CLIENT_EXTRACT_DIR = "."
CLIENT_EXE = "ServerTest/Abydos Record Test.exe"
LOCAL_XML_FILE = "version.xml"

class MainWindow(wx.Frame):
    def __init__(self, parent, title):
        wx.Frame.__init__(self, parent, title=title, size=(200, 100),
            style=wx.CAPTION | wx.CLOSE_BOX)

        self.launchButton = wx.Button(self, wx.ID_CLEAR, "Launch", pos=(50, 40))
        self.launchButton.Disable()
        self.Bind(wx.EVT_BUTTON, self.onLaunch, self.launchButton)

        self.label = wx.StaticText(self, label="Checking...", pos=(50, 20))
        
        self.progressBar = wx.Gauge(self, range=100, pos=(0, 0), size=(200, 20))
        
        self.Show(True)    

    def checkForUpdate(self):
        serverVersion, serverXML = self.getServerVersion()
        localVersion = self.getLocalVersion()
        
        if serverVersion != localVersion or os.path.isfile(CLIENT_EXE) == False:
            self.downloadClient()
            self.extractClientZip()
            self.writeLocalVersion(serverXML)

        self.progressBar.SetValue(self.progressBar.GetRange())
        self.launchButton.Enable()
        self.label.SetLabel("Ready to launch")

    def onLaunch(self, event): 
        try:
            self.platformIndependentLaunch()
        except OSError:
            # File has been manually removed, download again and relaunch
            self.downloadClient()
            self.platformIndependentLaunch()
            self.writeLocalVersion(serverXML)

    def platformIndependentLaunch(self):
        currentOS = platform.system()
        if currentOS == "Darwin":
            subprocess.call("ls")

    def downloadClient(self):
        self.label.SetLabel("Downloading...")
        self.progressBar.SetRange(100)
        self.progressBar.SetValue(0)
        clientURL = urllib2.urlopen(CLIENT_DOWNLOAD_URL)
        clientZipFile = open(CLIENT_ZIP_FILE, "wb")

        clientDownloadSize = self.getClientDownloadSize(clientURL)
        chunkSize = clientDownloadSize/self.progressBar.GetRange()
        bytesDownloaded = 0

        while bytesDownloaded < clientDownloadSize:
            clientZipFile.write(clientURL.read(chunkSize))
            bytesDownloaded += chunkSize
            self.progressBar.SetValue(self.progressBar.GetValue() + 1)
            wx.Yield()
     
    def extractClientZip(self):
        self.label.SetLabel("Extracting...")
        with zipfile.ZipFile(CLIENT_ZIP_FILE) as zf:
            members = zf.infolist()
            self.progressBar.SetRange(len(members))
            self.progressBar.SetValue(0)
            for member in zf.infolist():
                zf.extract(member, CLIENT_EXTRACT_DIR)   
                self.progressBar.SetValue(self.progressBar.GetValue() + 1)


    def getTotalZipFileSize(self, members):
        byteSize = 0
        for member in members:
            byteSize += member.file_size
        return byteSize

    def extractVersionNumber(self, xml):
        root = ET.fromstring(xml)
        return float(root[0].text)

    def getServerVersion(self):
        response = urllib2.urlopen(SERVER_XML_URL)
        xmlFile = response.read()
        return self.extractVersionNumber(xmlFile), xmlFile

    def getLocalVersion(self):
        try:
            with open(LOCAL_XML_FILE, 'r') as f:
                xml = f.read()
                return self.extractVersionNumber(xml)
        except IOError:
            return "0"
        
    def writeLocalVersion(self, serverXML):
        with open(LOCAL_XML_FILE, 'w') as f:
            return f.write(serverXML)
        
    def getClientDownloadSize(self, clientURL):
        meta = clientURL.info()
        return int(meta.getheaders("Content-Length")[0])

if __name__ == "__main__":
    app = wx.App(False)
    frame = MainWindow(None, "NextGen Launcher")
    frame.checkForUpdate()
    app.MainLoop()
