unit filename;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFrm_Filename }

  TFrm_Filename = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit_Filename: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Frm_Filename: TFrm_Filename;

implementation

{$R *.lfm}

{ TFrm_Filename }

procedure TFrm_Filename.FormShow(Sender: TObject);
begin
  Edit_Filename.SelStart := Edit_Filename.GetTextLen;
end;

end.

