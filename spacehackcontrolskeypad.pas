unit spacehackcontrolskeypad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, controls, Graphics,
  ExtCtrls, ComCtrls, Buttons, MQTTComponent, spacehackcontrols;

type
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

implementation

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

procedure TSpacehackGameControlKeypad.handleButtonClick(sender: TObject);
begin;
  //TODO handle keypad clicks
end;

end.

