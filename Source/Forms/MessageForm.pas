unit MessageForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TMessageForm1 = class(TForm)
    Editor: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MessageForm1, ResultForm, SummaryForm : TMessageForm1;

implementation

{$R *.DFM}

end.
