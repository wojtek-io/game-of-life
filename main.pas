unit Main;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  BGRAVirtualScreen,
  BGRABitmap,
  BGRABitmapTypes,
  Universe;

type

  { TFormGameOfLife }

  TFormGameOfLife = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    TimerRunStep: TTimer;
    TimerGarbageCollector: TTimer;
    procedure BGRAVirtualScreenMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreenMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreenMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure BGRAVirtualScreenMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure TimerRunStepTimer(Sender: TObject);
    procedure TimerGarbageCollectorTimer(Sender: TObject);
  private
    FUniverse: TUniverse;
    FName: String;
    FWidth: Integer;
    FHeight: Integer;
    FDragging: Boolean;
    FOrigX: Integer;
    FOrigY: Integer;
    FPosX: Integer;
    FPosY: Integer;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FCellSize: Integer;
    procedure OpenRleFile;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormGameOfLife: TFormGameOfLife;

implementation

uses
  Node,
  Rle;

{$R *.lfm}

{ TFormGameOfLife }

procedure TFormGameOfLife.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    'o', 'O':
    begin
      TimerRunStep.Enabled := False;
      FPosX := 0;
      FPosY := 0;
      OpenRleFile;
      BGRAVirtualScreen.RedrawBitmap;
    end;
    's', 'S':
    begin
      TimerRunStep.Enabled := False;
      FUniverse.RunStep;
      BGRAVirtualScreen.RedrawBitmap;
    end;
    ' ': TimerRunStep.Enabled := not TimerRunStep.Enabled;
    #27: Close;
  end;
  Key := #0;
end;

procedure TFormGameOfLife.TimerRunStepTimer(Sender: TObject);
begin
  FUniverse.RunStep;
  BGRAVirtualScreen.RedrawBitmap;
end;

procedure TFormGameOfLife.TimerGarbageCollectorTimer(Sender: TObject);
begin
  TNode.GarbageCollector;
end;

procedure TFormGameOfLife.OpenRleFile;
var
  Stream: TStream;
  RleParser: TRleParser;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := 'Select RLE pattern file';
    Options := [ofFileMustExist];
    Filter := 'RLE pattern file|*.rle';

    if not Execute then
      Exit;

    FUniverse.Clear;

    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      RleParser := TRleParser.Create(Stream);
      try
        FName := RleParser.Name;
        FWidth := RleParser.Width;
        FHeight := RleParser.Height;
        RleParser.LoadInUniverse(FUniverse, -(RleParser.Width div 2), (RleParser.Height div 2));
      finally
        FreeAndNil(RleParser);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    Free;
  end;
end;

constructor TFormGameOfLife.Create(TheOwner: TComponent);
begin
  inherited;
  FUniverse := TUniverse.Create;
  FCellSize := 2;
  OpenRleFile;
end;

destructor TFormGameOfLife.Destroy;
begin
  FUniverse.Free;
  inherited;
end;

procedure TFormGameOfLife.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);

  procedure DrawCells(const Node: INode; x, y: Integer);
  var
    Offset: Integer;
  begin
    if Node.Level = 0 then
    begin
      Bitmap.FillRect(x * FCellSize, y * FCellSize, (x + 1) * FCellSize, (y + 1) * FCellSize, BGRAWhite);
    end
    else
    begin
      Offset := 1 shl (Node.Level - 1);

      if Node.NW.Population > 0 then
         DrawCells(Node.NW, x - Offset, y + Offset);

      if Node.NE.Population > 0 then
         DrawCells(Node.NE, x + Offset, y + Offset);

      if Node.SW.Population > 0 then
         DrawCells(Node.SW, x - Offset, y - Offset);

      if Node.SE.Population > 0 then
         DrawCells(Node.SE, x + Offset, y - Offset);
    end;
  end;

begin
  DrawCells(FUniverse.Root, FPosX + FOffsetX + Bitmap.Width div (2 * FCellSize), FPosY + FOffsetY + Bitmap.Height div (2 * FCellSize));
  Bitmap.TextOut(5, 5, Format('Population: %d', [FUniverse.Root.Population]), BGRAWhite);
end;

procedure TFormGameOfLife.BGRAVirtualScreenMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      FDragging := True;
      FOrigX := X;
      FOrigY := Y;
    end;
    mbMiddle:
    begin
      FPosX := 0;
      FPosY := 0;
      BGRAVirtualScreen.RedrawBitmap;
    end;
  end;
end;

procedure TFormGameOfLife.BGRAVirtualScreenMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FDragging then
    Exit;

  FOffsetX := (X - FOrigX) div FCellSize;
  FOffsetY := (Y - FOrigY) div FCellSize;
  BGRAVirtualScreen.RedrawBitmap;
end;

procedure TFormGameOfLife.BGRAVirtualScreenMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      FDragging := False;
      FPosX := FPosX + FOffsetX;
      FPosY := FPosY + FOffsetY;
      FOffsetX := 0;
      FOffsetY := 0;
      BGRAVirtualScreen.RedrawBitmap;
    end;
  end;
end;

procedure TFormGameOfLife.BGRAVirtualScreenMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Dec(FCellSize);

  if FCellSize < 1 then
    FCellSize := 1;

  BGRAVirtualScreen.RedrawBitmap;
end;

procedure TFormGameOfLife.BGRAVirtualScreenMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Inc(FCellSize);

  if FCellSize > 16 then
    FCellSize := 16;

  BGRAVirtualScreen.RedrawBitmap;
end;

end.

