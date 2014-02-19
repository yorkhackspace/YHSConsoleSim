unit spacehackcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, controls, Graphics,
  ExtCtrls, ComCtrls, Buttons, MQTTComponent;

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
    procedure updateUI virtual;
  end;

  TSPacehackInstructionDisplay = class(TSpacehackControl)
    instruction: string;
    procedure initUI(thisPanel:TPanel) override;
    procedure updateUI override;
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
    procedure handleButtonClick(sender: TObject);
  end;

  TSpacehackGameControlIlluminatedToggle = class(TSpacehackGameControl)
    lightIsOn: boolean;
    lblLight: TLabel;
    btnOn, btnOff: TButton;
    panelWidth: integer;
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
    procedure updateUI override;
    procedure handleOnClick(Sender: TObject);
    procedure handleOffClick(Sender: TObject);
  end;

  TSpacehackGameControlFourButtons = class(TSpacehackGameControl)
    button1Name, button2Name, button3Name, button4Name: string;
    Button1, Button2, Button3, Button4: TButton;
    procedure initUI(thisPanel:TPanel) override;
    procedure handleButtonClick(sender: TObject);
  end;

  TSpacehackGameControlPotentiometer = class(TSpacehackGameControl)
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
  end;

  TSpacehackGameControlIlluminatedButton = class(TSpacehackGameControl)
    lightIsOn: boolean;

    lblLight: TLabel;
    theButton: TButton;
    panelWidth: integer;

    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
    procedure handleButtonUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure handleButtonDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure updateUI override;
  end;

  TSpacehackGameControlCombo7SegColourRotary = class(TSpacehackGameControl)
    procedure update(action: TSpacehackControlAction; myTopic: string; value: integer) override;
    procedure initUI(thisPanel:TPanel) override;
  end;

var
  MQTTClient: TMQTTClient;

implementation

{ Spacehack controls }

//Default update handler, just sends the tag value to the correct update topic
procedure TSpacehackGameControl.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaUp:  MQTTClient.Publish(myTopic, inttostr(value));
  end;
end;

procedure TSpacehackGameControlIlluminatedButton.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown: begin MQTTClient.Publish(myTopic, '1'); self.lightIsOn:=true; end;
       shcaUp: begin MQTTClient.Publish(myTopic, '0');   self.lightIsOn:=false; end;
  end;
end;

procedure TSpacehackGameControlIlluminatedToggle.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown: begin
         MQTTClient.Publish(myTopic, '1');
         self.lightIsOn:=true;
       end;
       shcaUp: begin
         MQTTClient.Publish(myTopic, '0');
         self.lightIsOn:=false;
       end;
  end;
end;

procedure TSpacehackGameControlCombo7SegColourRotary.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown:  MQTTClient.Publish(myTopic, '1');
       shcaUp:  MQTTClient.Publish(myTopic, '0');
  end;
end;


procedure TSpacehackGameControlPotentiometer.update(action: TSpacehackControlAction; myTopic: string; value: integer);
begin
  case action of
       shcaDown:  MQTTClient.Publish(myTopic, '1');
       shcaUp:  MQTTClient.Publish(myTopic, '0');
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
    width := thisPanel.Width - 4;
  end;
  updateUI;
end;

procedure TSpacehackGameControlIlluminatedButton.initUI(thisPanel: TPanel);
begin;
  inherited;
  lblLight := TLabel.Create(thisPanel);
  panelWidth := thisPanel.Width;
  with lblLight do begin
    top := 40;
    left := 0;
    AutoSize:=true;
    Font.Size:=20;
    Parent:= thisPanel;
    Visible:=true;
  end;
  updateUI;
  theButton := TButton.Create(thisPanel);
  with theButton do begin
    width := 100;
    height := 100;
    top := 90;
    left := 50;
    parent := thisPanel;
    Visible:=true;
    tag := 1;
    OnMouseDown:= @handleButtonDown;
    OnMouseUp:= @handleButtonUp;
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
              OnClick:= @handleButtonClick;
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
  panelWidth := thisPanel.Width;
  with lblLight do begin
    top := 40;
    left := 0;
    AutoSize:=true;
    Font.Size:=20;
    Parent:= thisPanel;
    Visible:=true;
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
    OnClick:=@handleOnClick;
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
    OnClick:=@handleOffClick;
  end;
  updateUI;
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
              parent := thisPanel; visible := true;tag := buttonTag;
              OnClick:= @handleButtonClick;
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
 //UI event handlers
procedure TSpacehackGameControlIlluminatedButton.handleButtonUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lightIsOn := false;
  ;
end;

procedure TSpacehackGameControlIlluminatedButton.handleButtonDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin;
  lightIsOn := true;
end;

procedure TSpacehackGameControlIlluminatedToggle.handleOnClick(Sender: TObject);
begin;
  lightIsOn:=true;
end;

procedure TSpacehackGameControlIlluminatedToggle.handleOffClick(Sender: TObject);
begin;
  lightIsOn:=false;
end;

procedure TSpacehackGameControlFourButtons.handleButtonClick(sender: TObject);
begin;
  //TODO handle four buttons
end;

procedure TSpacehackGameControlKeypad.handleButtonClick(sender: TObject);
begin;
  //TODO handle keypad clicks
end;

//UI updaters

procedure TSpacehackControl.updateUI;
begin
  //blah
end;

procedure TSPacehackInstructionDisplay.updateUI;
begin;
  aLabel.Caption:=instruction;
end;

procedure TSpacehackGameControlIlluminatedButton.updateUI;
begin
  with lblLight do
  begin;
    if lightIsOn then
    begin
      Caption:= 'ON';
      Color:=clGreen;
    end else
    begin
      Caption:= 'OFF';
      Color:=clRed;
    end;
    left := round((panelWidth/2) - (width/2));
  end;
end;

procedure TSpacehackGameControlIlluminatedToggle.updateUI;
begin
  with lblLight do begin
    if lightIsOn then
    begin
      Caption:= 'ON';
      Color:=clGreen;
      btnOff.Enabled:=true;
      btnOn.Enabled:=false;
    end else
    begin
      Caption:= 'OFF';
      Color:=clRed;
      btnoff.Enabled:=false;
      btnOn.Enabled:=true;
    end;
    left := round((panelWidth/2) - (width/2));
  end;
end;

end.

