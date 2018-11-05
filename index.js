
'use strict'
const electron = require('electron')

const app = electron.app // this is our app

// Uncomment this for testing with self-signed certificates
app.commandLine.appendSwitch('ignore-certificate-errors', 'true');

const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows

const Menu = electron.Menu;

let mainWindow // saves a global reference to mainWindow so it doesn't get garbage collected

app.on('ready', createWindow) // called when electron has initialized

// This will create our app window, no surprise there
function createWindow () {
  mainWindow = new BrowserWindow({
    width: 1024,
    height: 768
  });

  // display the index.html file
  mainWindow.loadURL(`file://${ __dirname }/electron-index.html`);

  // open dev tools by default so we can see any console errors
  // mainWindow.webContents.openDevTools()

  mainWindow.on('closed', function () {
    mainWindow = null
  });

  // Create the Application's main menu
  var template = [{
    label: "Exosphere",
    submenu: [
      { label: "About Exosphere", selector: "orderFrontStandardAboutPanel:" },
      { type: "separator" },
      { label: "Quit", accelerator: "Command+Q", click: function() { app.quit(); }}
    ]}, {
    label: "Edit",
    submenu: [
      { label: "Undo", accelerator: "CmdOrCtrl+Z", selector: "undo:" },
      { label: "Redo", accelerator: "Shift+CmdOrCtrl+Z", selector: "redo:" },
      { type: "separator" },
      { label: "Cut", accelerator: "CmdOrCtrl+X", selector: "cut:" },
      { label: "Copy", accelerator: "CmdOrCtrl+C", selector: "copy:" },
      { label: "Paste", accelerator: "CmdOrCtrl+V", selector: "paste:" },
      { label: "Select All", accelerator: "CmdOrCtrl+A", selector: "selectAll:" }
    ]}
  ];

  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
