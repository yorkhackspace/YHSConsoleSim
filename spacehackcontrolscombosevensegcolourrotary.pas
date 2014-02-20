unit spacehackcontrolscombosevensegcolourrotary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, controls, Graphics,
  ExtCtrls, ComCtrls, Buttons, MQTTComponent, spacehackcontrols;

type
  TSpacehackGameControlCombo7SegColourRotary = class(TSpacehackGameControl)
    procedure initUI(thisPanel:TPanel) override;
  end;

implementation

procedure TSpacehackGameControlCombo7SegColourRotary.initUI(thisPanel: TPanel);
begin;
  inherited;

end;

end.

