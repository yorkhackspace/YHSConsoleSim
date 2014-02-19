unit consolesimMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MaskEdit, ComCtrls, EditBtn, Buttons, MQTTComponent, fpjson,
  JSONParser;

type

  TSpacehackControlAction = (shcaUp, shcaDown);

  TSpacehackDisplay = record
    displayType: string;
    charWidth, charHeight: integer;
  end;

  TSpacehackPins = record
    numPins: integer;
    pinNames: array of string;
    pinIDs: array of string;
  end;

  TSpacehackBus = record
    busName: string;
    pins: TSpacehackPins;
  end;

  TSpacehackControl = class(TObject)
    display: TSpacehackDisplay;
    hardware: string;
    pins: TSpacehackPins;
    name: string;

    //ui stuff
    aLabel: TLabel;

    procedure initUI(thisPanel:TPanel) virtual;
  end;

  TSPacehackInstructionDisplay = class(TSpacehackControl)
    instruction: string;
    procedure initUI(thisPanel:TPanel) override;
  end;

  TSpacehackGameControl = class(TSpacehackControl)
    public
    enabled: boolean;
    procedure update(action: TSpacehackControlAction; myTopic: string;  value: integer) virtual;
  end;

  TSpacehackGameControlKeypad = class(TSpacehackGameControl)
    Button0,Button1,Button2,
    Button3,Button4,Button5,
    Button6,Button7,Button8,
    Button9,ButtonStar, ButtonHash,
    ButtonA, ButtonB, ButtonC, ButtonD
    : TButton;
    procedure initUI(thisPanel:TPanel) override;
  end;

  TSpacehackGameControlIlluminatedToggle = class(TSpacehackGameControl)
    lightIsOn: boolean;
    lblLight: TLabel;
    btnOn, btnOff: TButton;
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
  end;

  TSpacehackGameControlFourButtons = class(TSpacehackGameControl)
    button1Name, button2Name, button3Name, button4Name: string;
    Button1, Button2, Button3, Button4: TButton;
    procedure initUI(thisPanel:TPanel) override;
  end;

  TSpacehackGameControlPotentiometer = class(TSpacehackGameControl)
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
  end;

  TSpacehackGameControlIlluminatedButton = class(TSpacehackGameControl)
    lightIsOn: boolean;

    lblLight: TLabel;
    theButton: TButton;

    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
  end;

  TSpacehackGameControlCombo7SegColourRotary = class(TSpacehackGameControl)
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
  end;



  { TfrmMain }

  TfrmMain = class(TForm)
    btnSubscribe: TButton;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnRegister: TButton;
    btnReload: TButton;
    btnAutoSubscribe: TButton;
    btnClearSubs: TButton;
    btnCreateControls: TButton;
    btnPublish: TButton;
    edtPublishTopic: TEdit;
    eServer: TEdit;
    eSubscription: TEdit;
    ePort: TMaskEdit;
    fneLoadConfig: TFileNameEdit;
    gbServer: TGroupBox;
    gbSub: TGroupBox;
    gbStdSub: TGroupBox;
    gbAddSub: TGroupBox;
    gbLoadConfig: TGroupBox;
    gbControls: TGroupBox;
    gbComServer: TGroupBox;
    gbComLocal: TGroupBox;
    gbSend: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblServerIP: TLabel;
    lblMyIP: TLabel;
    memAddSub: TMemo;
    memPublishPayload: TMemo;
    memStdSub: TMemo;
    MQTTClient: TMQTTClient;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlControls: TPanel;
    pnlLoadConfig: TPanel;
    sbDrawingArea: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    statusBar: TStatusBar;
    tmrStat: TTimer;
    tvControls: TTreeView;
    procedure btnAutoSubscribeClick(Sender: TObject);
    procedure btnClearSubsClick(Sender: TObject);
    procedure btnCreateControlsClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);

    procedure handleCtrlUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure handleCtrlDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);

    procedure fneLoadConfigAcceptFileName(Sender: TObject; var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbStdSubClick(Sender: TObject);
    procedure log(info: string);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSubscribeClick(Sender: TObject);
    procedure MQTTClientConnAck(Sender: TObject; ReturnCode: integer);
    procedure MQTTClientPingResp(Sender: TObject);
    procedure MQTTClientPublish(Sender: TObject; topic, payload: ansistring);
    procedure MQTTClientSubAck(Sender: TObject; MessageID: integer;
      GrantedQoS: integer);
    procedure pnlLoadConfigResize(Sender: TObject);
    procedure sbDrawingAreaPaint(Sender: TObject);
    procedure tmrStatTimer(Sender: TObject);
    procedure loadControlData;
    procedure controlDataToTree;
    procedure loadConfiguration(configFile: string);
    procedure subscribeTo(topic: string; isAdditional: boolean = false);
    procedure setInstruction(instruction: string);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
  myJSONParser: TJSONParser;
  localConfigJSON, interfaceConfigJSON: TJSONObject;
  controlsJSON, busesJSON : TJSONObject;
  myIP, serverIP: string;
  numControls, numBuses: integer;
  controlJSON, busJSON: array of TJSONObject;
  controlID: array of integer;
  busID: array of string;
  spacehackControls: array of TSpacehackControl;
  spacehackBuses: array of TSpacehackBus;

  nextTop, nextLeft: integer;

implementation

{$R *.lfm}

{ Spacehack controls }

//Default update handler, just sends the tag value to the correct update topic
procedure TSpacehackGameControl.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaUp:  frmMain.MQTTClient.Publish(myTopic, inttostr(value));
  end;
end;

procedure TSpacehackGameControlIlluminatedButton.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown: begin frmMain.MQTTClient.Publish(myTopic, '1'); self.lightIsOn:=true; end;
       shcaUp: begin frmMain.MQTTClient.Publish(myTopic, '0');   self.lightIsOn:=false; end;
  end;
end;

procedure TSpacehackGameControlIlluminatedToggle.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown: begin
         frmMain.MQTTClient.Publish(myTopic, '1');
         self.lightIsOn:=true;
       end;
       shcaUp: begin
         frmMain.MQTTClient.Publish(myTopic, '0');
         self.lightIsOn:=false;
       end;
  end;
end;

procedure TSpacehackGameControlCombo7SegColourRotary.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown:  frmMain.MQTTClient.Publish(myTopic, '1');
       shcaUp:  frmMain.MQTTClient.Publish(myTopic, '0');
  end;
end;


procedure TSpacehackGameControlPotentiometer.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown:  frmMain.MQTTClient.Publish(myTopic, '1');
       shcaUp:  frmMain.MQTTClient.Publish(myTopic, '0');
  end;
end;

//UI initiators

procedure TSpacehackControl.initUI(thisPanel: TPanel);
begin;
  aLabel := TLabel.Create(thisPanel);
  with aLabel do begin
    top := 20;
    left := 0;
    Font.Size:=20;
    Parent:= thisPanel;
    Visible:=true;
    caption := name;
  end;
end;

procedure TSPacehackInstructionDisplay.initUI(thisPanel: TPanel);
begin;
  inherited;
  with aLabel do begin
    WordWrap:=true;
    AutoSize:=true;
    Caption:=self.instruction;
    width := thisPanel.Width - 4;
  end;
end;

procedure TSpacehackGameControlIlluminatedButton.initUI(thisPanel: TPanel);
begin;
  inherited;
  lblLight := TLabel.Create(thisPanel);
  with lblLight do begin
    top := 40;
    left := 0;
    AutoSize:=true;
    Font.Size:=20;
    Parent:= thisPanel;
    Visible:=true;
    if lightIsOn then
    begin
      Caption:= 'ON';
      Color:=clGreen;
    end else
    begin
      Caption:= 'OFF';
      Color:=clRed;
    end;
    left := round((thisPanel.Width/2) - (width/2));
  end;

  theButton := TButton.Create(thisPanel);
  with theButton do begin
    width := 100;
    height := 100;
    top := 90;
    left := 50;
    parent := thisPanel;
    Visible:=true;
    tag := 1;
    OnMouseDown:= @frmMain.handleCtrlDown;
    OnMouseUp:= @frmMain.handleCtrlUp;
  end;
end;

procedure TSpacehackGameControlFourButtons.initUI(thisPanel: TPanel);
          procedure initButton(button: TButton; panel: TPanel; buttontop: integer; buttonTag: integer; buttonname: string);
          begin;
            button := TButton.Create(panel);
            with button do begin
              left := 5; top := buttontop; width := 190; height := 35;
              caption := buttonname;
              parent := thisPanel; visible := true;
              tag := buttonTag;
              OnMouseDown:= @frmMain.handleCtrlDown;
              OnMouseUp:= @frmMain.handleCtrlUp;
            end;
          end;
begin;
  inherited;
  initButton(Button1, thisPanel, 40, 1, self.button1Name);
  initButton(Button2, thisPanel, 75, 2, self.button2Name);
  initButton(Button3, thisPanel, 110, 3, self.button3Name);
  initButton(Button4, thisPanel, 145, 4, self.button4Name);
end;

procedure TSpacehackGameControlIlluminatedToggle.initUI(thisPanel: TPanel);
begin;
  inherited;
  lblLight := TLabel.Create(thisPanel);
  with lblLight do begin
    top := 40;
    left := 0;
    AutoSize:=true;
    Font.Size:=20;
    Parent:= thisPanel;
    Visible:=true;
    if lightIsOn then
    begin
      Caption:= 'ON';
      Color:=clGreen;
    end else
    begin
      Caption:= 'OFF';
      Color:=clRed;
    end;
    left := round((thisPanel.Width/2) - (width/2));
  end;
  btnOn := TButton.Create(thisPanel);
  with btnOn do begin
    width := 100;
    height := 50;
    top := 90;
    left := 50;
    parent := thisPanel;
    Visible:=true;
    Caption:='On';
    tag := 1;
    OnMouseDown:= @frmMain.handleCtrlDown;
    OnMouseUp:= @frmMain.handleCtrlUp;
  end;
  btnOff := TButton.Create(thisPanel);
  with btnOff do begin
    width := 100;
    height := 50;
    top := 140;
    left := 50;
    parent := thisPanel;
    Visible:=true;
    Caption:='Off';
    tag := 0;
    OnMouseDown:= @frmMain.handleCtrlDown;
    OnMouseUp:= @frmMain.handleCtrlUp;
  end;
  if lightIsOn then
  begin
    btnOff.Enabled:=true;
    btnOn.Enabled:=false;
  end else
  begin
    btnoff.Enabled:=false;
    btnOn.Enabled:=true;
  end;
end;

procedure TSpacehackGameControlCombo7SegColourRotary.initUI(thisPanel: TPanel);
begin;
  inherited;

end;

procedure TSpacehackGameControlKeypad.initUI(thisPanel: TPanel);
          procedure initButton(button: TButton; panel: TPanel; buttonX, buttonY: integer; buttonTag: integer; buttonname: string);
          const
            xVals: array [1..4] of integer = (5,55,105,155);
            yVals: array [1..4] of integer = (40,80,120,160);
          begin;
            button := TButton.Create(panel);
            with button do begin
              left := xVals[buttonX]; top := yVals[buttonY]; width := 40; height := 40;
              caption :=buttonName;
              parent := thisPanel; visible := true;tag := buttonTag;OnMouseDown:= @frmMain.handleCtrlDown;OnMouseUp:= @frmMain.handleCtrlUp;
            end;
          end;
begin;
  inherited;
  initButton(Button1, thisPanel, 1, 1, 1, '1');
  initButton(Button4, thisPanel, 1, 2, 4, '4');
  initButton(Button7, thisPanel, 1, 3, 7, '7');
  initButton(ButtonStar, thisPanel, 1, 4, 101, '*');

  initButton(Button2, thisPanel, 2, 1, 2, '2');
  initButton(Button5, thisPanel, 2, 2, 5, '5');
  initButton(Button8, thisPanel, 2, 3, 8, '8');
  initButton(Button0, thisPanel, 2, 4, 0, '0');

  initButton(Button3, thisPanel, 3, 1, 3, '3');
  initButton(Button6, thisPanel, 3, 2, 6, '6');
  initButton(Button9, thisPanel, 3, 3, 9, '9');
  initButton(ButtonHash, thisPanel, 3, 4, 102, '#');

  initButton(ButtonA, thisPanel, 4, 1, 103, 'A');
  initButton(ButtonB, thisPanel, 4, 2, 104, 'B');
  initButton(ButtonC, thisPanel, 4, 3, 105, 'C');
  initButton(ButtonD, thisPanel, 4, 4, 106, 'D');
end;

{ TfrmMain }

procedure TfrmMain.log(info: string);
begin
//  mOutputOld.Append(info);
//  mOutputOld.Text:=mOutputOld.Text + #10 + info;
  //mOutput.Lines.Add(info);
  writeln(info);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  MQTTClient.init;
end;

procedure TfrmMain.gbStdSubClick(Sender: TObject);
begin

end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MQTTClient.deInit;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  btnClearSubs.Click;
  gbComServer.Enabled:=false;
  log('Disconnecting...');
  if MQTTClient.isConnected then begin
    log('Graceful disconnect...');
    if not MQTTClient.Disconnect then begin
      log('Forecful disconnect...');
      MQTTClient.ForceDisconnect;
    end;
  end;
end;

procedure TfrmMain.btnPublishClick(Sender: TObject);
begin
  MQTTClient.Publish(edtPublishTopic.Text, memPublishPayload.Text);
end;

procedure TfrmMain.btnRegisterClick(Sender: TObject);
begin
  MQTTClient.Publish('server/register', interfaceConfigJSON.FormatJSON());
end;

procedure TfrmMain.subscribeTo(topic: string; isAdditional: boolean = false);
begin
  MQTTClient.Subscribe(topic);
  if isAdditional then begin
    memAddSub.Append(topic);
  end else
    memStdSub.Append(topic);
end;

procedure TfrmMain.btnAutoSubscribeClick(Sender: TObject);
var
  prefix: string;
  i: integer;
begin
  prefix := 'clients/' + myIP + '/';
  subscribeTo(prefix + 'configure');
  subscribeTo(prefix + 'instructions');

  for i := 0 to numControls-1 do
  begin
    if spacehackControls[i].hardware <> 'instructions' then begin
      subscribeTo(prefix + inttostr(controlID[i]) + '/name');
      subscribeTo(prefix + inttostr(controlID[i]) + '/enabled');
    end;
  end;
end;

procedure TfrmMain.btnClearSubsClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to memStdSub.Lines.Count-1 do begin
    MQTTClient.Unsubscribe(memStdSub.Lines[i]);
  end;
  memStdSub.Clear;
  for i := 0 to memAddSub.Lines.Count-1 do begin
    MQTTClient.Unsubscribe(memAddSub.Lines[i]);
  end;
  memAddSub.Clear;
end;


procedure handleGameControl(action: TSpacehackControlAction; sender: TObject);
var
  controlID: integer;
  stateID: integer;
begin
  controlID := TWinControl(sender).Parent.Tag;
  stateID := TWinControl(Sender).Tag;
  frmMain.log(inttostr(stateID) + ' Changed for control ' + inttostr((controlID)));
  frmMain.log(spacehackControls[controlID].ToString);
  TSpacehackGameControl(spacehackControls[controlID]).update(action, 'clients/' + myIP + '/' + inttostr(controlID) + '/valuechanged', stateID);
end;

procedure TfrmMain.handleCtrlDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
begin
  handleGameControl(shcaDown, sender);
end;

procedure TfrmMain.handleCtrlUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
begin
  handleGameControl(shcaUp, sender);
end;

procedure TfrmMain.btnCreateControlsClick(Sender: TObject);
var
  i: integer;
  thisControl: TSpacehackControl;
  thisPanel: TPanel;
  thisLabel: TLabel;
begin
  for i := pnlControls.ControlCount-1 downto 0 do begin
    pnlControls.Controls[i].Free;
  end;
  pnlControls.Width:=sbDrawingArea.Width - 20;
  pnlControls.Height:=220;
  nextTop := 10;
  nextLeft := 10;
  for i := 0 to numControls-1 do
  begin
    thisControl := spacehackControls[i];
    thisPanel := TPanel.Create(pnlControls);
    with thisPanel do begin
      Width:=200;
      height:=200;
      Top:=nextTop;
      Left:=nextLeft;
      Parent:= pnlControls;
      Visible:=true;
    end;
    thisLabel := TLabel.Create(thisPanel);
    with thisLabel do begin
      top := 0;
      left := 0;
      caption := spacehackControls[i].hardware;
      parent := TWinControl(thisPanel);
      visible := true;
    end;
    thisPanel.Tag:=i;
    thisControl.initUI(thisPanel);
    nextLeft += 210;
    if nextLeft+210 >= pnlControls.Width then
    begin
      nextLeft := 10;
      nextTop += 210;
      pnlControls.Height := pnlControls.Height + 210;
    end;
  end;
end;

procedure TfrmMain.btnReloadClick(Sender: TObject);
begin
  loadConfiguration(fneLoadConfig.Text);
end;

procedure TfrmMain.fneLoadConfigAcceptFileName(Sender: TObject;
  var Value: String);

begin
  loadConfiguration(Value);

end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  log('Starting connection...');
  if MQTTClient.isConnected then begin
    if not MQTTClient.Disconnect then begin
      MQTTClient.ForceDisconnect;
    end;
  end;
  log('Host: ' + eServer.Text);
  log('Port: ' + trim(ePort.Text));
  log('CID: ' + MQTTClient.ClientID);
  MQTTClient.Hostname:=eServer.Text;
  MQTTClient.Port:=strToInt(trim(ePort.Text));
  log('Waiting for connection...');
  MQTTClient.Connect;
end;

procedure TfrmMain.btnSubscribeClick(Sender: TObject);
var
  subscription: string;
begin
  subscribeTo(eSubscription.Text, true);
end;

procedure TfrmMain.MQTTClientConnAck(Sender: TObject; ReturnCode: integer);
begin
  log('Connection established, returned: '+ inttostr(ReturnCode));
  gbComServer.Enabled:=true;
end;

procedure TfrmMain.MQTTClientPingResp(Sender: TObject);
begin
  //log('Ping response');
end;

procedure TfrmMain.setInstruction(instruction: string);
var
  i: integer;
begin
  for i := 0 to numControls-1 do
  begin
    if spacehackControls[i].hardware = 'instructions' then begin
      log('Instruction update');
      TSPacehackInstructionDisplay(spacehackControls[i]).instruction:=instruction;
    end;
  end;
end;

procedure TfrmMain.MQTTClientPublish(Sender: TObject; topic, payload: ansistring
  );
var
  topicParser: TStrings;
begin
  log(topic + ': ' + payload);
  topicParser := TStringList.Create;
  topicParser.Delimiter:='/';
  topicParser.DelimitedText:=topic;
  if  (topicParser.Count > 0) and (topicParser.Strings[0] <> 'clients') then begin
    log('Non-client topic received.');
  end else
  begin
    if topicParser.Count > 1 then begin
      if topicParser.Strings[1] <> myIP then begin
        log('IP address mismatch');
      end else
      begin
        //yay, my message!
        if topicParser.Count > 2 then begin
          if topicParser.Strings[2] = 'instructions' then begin
            log('next instruction is '+payload);
            setInstruction(payload);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.MQTTClientSubAck(Sender: TObject; MessageID: integer;
  GrantedQoS: integer);
begin
  log('Subscription established');
end;

procedure TfrmMain.pnlLoadConfigResize(Sender: TObject);
begin
  fneLoadConfig.Width:=pnlLoadConfig.Width-26;
end;

procedure TfrmMain.sbDrawingAreaPaint(Sender: TObject);
begin

end;

procedure TfrmMain.tmrStatTimer(Sender: TObject);
begin
  if MQTTClient.isConnected then begin
    statusBar.SimpleText:='Connected';
    gbComServer.Enabled:=true;
  end else
  begin
    statusBar.SimpleText:='Disconnected';
    gbComServer.Enabled:=false;
  end;
  MQTTClient.PingReq;
end;

procedure TfrmMain.loadControlData();
var
  i, j: integer;
  thisDisplayJSON, thisPinsJSON: TJSONObject;
  hardwareType : string;
begin
  //Get the number of controls by the number of children of the controls JSON object
  numControls := controlsJSON.Count;
  setLength(controlJSON,            numControls);
  setLength(controlID,              numControls);
  setLength(spacehackControls,      numControls);

  //Loop through all controls
  for i := 0 to numControls-1 do begin
    //get a JSON object for each one
    controlJSON[i] := TJSONObject(controlsJSON.find(inttostr(i)));
    //set its ID (maybe not needed)
    controlID[i] := i;
    //store the hardware type
    hardwareType := controlJSON[i].Find('hardware').AsString;
    if hardwareType = 'instructions' then spacehackControls[i] := TSPacehackInstructionDisplay.create;
    if hardwareType = 'combo7SegColourRotary' then spacehackControls[i] := TSpacehackGameControlCombo7SegColourRotary.create;
    if hardwareType = 'illuminatedtoggle' then spacehackControls[i] := TSpacehackGameControlIlluminatedToggle.create;
    if hardwareType = 'fourbuttons' then spacehackControls[i] := TSpacehackGameControlFourButtons.create;
    if hardwareType = 'potentiometer' then spacehackControls[i] := TSpacehackGameControlPotentiometer.create;
    if hardwareType = 'illuminatedbutton' then spacehackControls[i] := TSpacehackGameControlIlluminatedButton.create;
    if hardwareType = 'keypad' then spacehackControls[i] := TSpacehackGameControlKeypad.create;
    //spacehackControls[i] := TSpacehackControl.create;
    spacehackControls[i].hardware:=hardwareType;

    //get the JSON for the display object
    thisDisplayJSON := TJSONObject(controlJSON[i].Find('display'));
    //get the display information
    spacehackControls[i].display.displayType:=thisDisplayJSON.Find('type').AsString;
    spacehackControls[i].display.charHeight:=thisDisplayJSON.Find('height').AsInteger;
    spacehackControls[i].display.charWidth:=thisDisplayJSON.Find('width').AsInteger;

    //get the JSON for the pins object
    thisPinsJSON := TJSONObject(controlJSON[i].Find('pins'));
    //get the display information
    spacehackControls[i].pins.numPins:=thisPinsJSON.Count;
    setLength(spacehackControls[i].pins.pinNames, thisPinsJSON.Count);
    setLength(spacehackControls[i].pins.pinIDs, thisPinsJSON.Count);

    for j := 0 to spacehackControls[i].pins.numPins-1 do
    begin
      spacehackControls[i].pins.pinNames[j] := thisPinsJSON.Names[j];
      spacehackControls[i].pins.pinIDs[j] := thisPinsJSON.Strings[thisPinsJSON.Names[j]];
    end;

  end;



  //Get the number of controls by the number of children of the controls JSON object
  numBuses := busesJSON.Count;
  setLength(busJSON,            numControls);
  setLength(busID,              numControls);
  setLength(spacehackBuses,      numControls);

  //Loop through all controls
  for i := 0 to numBuses-1 do begin
    //set its ID
    busID[i] := busesJSON.Names[i];
    //get a JSON object for each one
    busJSON[i] := TJSONObject(busesJSON.Objects[busID[i]]);
    //set the bus name
    spacehackBuses[i].busName := busID[i];

    //get the JSON for the pins object
    thisPinsJSON := TJSONObject(busJSON[i]);

    //get the pin information
    spacehackBuses[i].pins.numPins:=thisPinsJSON.Count;
    setLength(spacehackBuses[i].pins.pinNames, thisPinsJSON.Count);
    setLength(spacehackBuses[i].pins.pinIDs, thisPinsJSON.Count);

    for j := 0 to spacehackBuses[i].pins.numPins-1 do
    begin
      spacehackBuses[i].pins.pinNames[j] := thisPinsJSON.Names[j];
      spacehackBuses[i].pins.pinIDs[j] := thisPinsJSON.Strings[thisPinsJSON.Names[j]];
    end;

  end;
end;

procedure TfrmMain.controlDataToTree;
var
  rootControlNode, rootBusNode: TTreeNode;
  thisNode, thisDisplay, thisPins: TTreeNode;
  i, j: integer;
begin

  tvControls.Items.Clear;

  rootControlNode := tvControls.Items.Add(nil, 'Controls');
  rootBusNode := tvControls.Items.Add(nil, 'Buses');
  for i := 0 to numControls-1 do
  begin
    thisNode := tvControls.Items.AddChild(rootControlNode, inttostr(controlID[i]));
    tvControls.Items.AddChild(thisNode, 'hardware: '+ spacehackControls[i].hardware);
    thisDisplay := tvControls.Items.AddChild(thisNode, 'display');
    tvControls.Items.AddChild(thisDisplay, 'Type: ' + spacehackControls[i].display.displayType);
    tvControls.Items.AddChild(thisDisplay, 'Width: ' + inttostr(spacehackControls[i].display.charWidth));
    tvControls.Items.AddChild(thisDisplay, 'Height: ' + inttostr(spacehackControls[i].display.charHeight));
    thisPins := tvControls.Items.AddChild(thisNode, 'pins');
    for j := 0 to spacehackControls[i].pins.numPins-1 do
    begin
      tvControls.Items.AddChild(thisPins, 'Pin name: ' + spacehackControls[i].pins.pinNames[j] + '   =>   ID: ' + spacehackControls[i].pins.pinIDs[j]);
    end;
  end;
  for i := 0 to numBuses-1 do
  begin
    thisNode := tvControls.Items.AddChild(rootBusNode, (busID[i]));

    for j := 0 to spacehackBuses[i].pins.numPins-1 do
    begin
      tvControls.Items.AddChild(thisNode, 'Pin name: ' + spacehackBuses[i].pins.pinNames[j] + '   =>   ID: ' + spacehackBuses[i].pins.pinIDs[j]);
    end;

  end;
end;

procedure TfrmMain.loadConfiguration(configFile: string);
var
  myFile: TFileStream;
  myJSONRoot: TJSONObject;
begin
  myFile := TFileStream.Create(configFile, fmOpenRead);
  if myFile = nil then begin
    showmessage('File load failed.');
  end else
  begin
    try
      myJSONParser := TJSONParser.Create(myFile);
      myJSONRoot := TJSONObject(myJSONParser.Parse);
      localConfigJSON := TJSONObject(myJSONRoot.Find('local'));
      interfaceConfigJSON := TJSONObject(myJSONRoot.Find('interface'));

      controlsJSON := TJSONObject(localConfigJSON.Find('controls'));
      busesJSON := TJSONObject(localConfigJSON.Find('buses'));

      myIP := interfaceConfigJSON.Find('ip').AsString;
      serverIP := localConfigJSON.Find('server').AsString;

      eServer.Text := serverIP;

      lblMyIP.Caption := 'My IP: ' + myIP;
      lblServerIP.Caption := 'Server IP:' + serverIP;
    except
      showmessage('JSON Parsing failed');
    end;
    myFile.Free;
  end;

  loadControlData;
  controlDataToTree;
end;

end.

