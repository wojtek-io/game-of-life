unit Rle;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes,
  SysUtils,
  Universe,
  streamex;

type

  { TRleParser }

  TRleParser = class
  private
    FStreamReader: TStreamReader;
    FName: string;
    FComment: string;
    FWidth: Integer;
    FHeight: Integer;
    FRule: string;
    function ReadInteger(var P: PChar): Integer;
  public
    constructor Create(const Stream: TStream); reintroduce;
    destructor Destroy; override;
    procedure LoadInUniverse(const Universe: TUniverse; const AX, AY: Integer);
    property Name: string read FName;
    property Comment: string read FComment;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Rule: String read FRule;
  end;

implementation

{ TRleParser }

function TRleParser.ReadInteger(var P: PChar): Integer;
begin
  Result := 0;

  while P^ = ' ' do
    Inc(P);

  while (P^ <> #0) do
  begin
    case P^ of
      '0'..'9': Result := Result * 10 + (Ord(P^) - Ord('0'));
      else Break;
    end;

    Inc(P);
  end;
end;

constructor TRleParser.Create(const Stream: TStream);

  function GetNextIdentifier(var P: PChar): string;
  begin
    Result := '';

    while P^ = ' ' do
      Inc(P);

    while not (P^ in [#0, ' ', '=']) do
    begin
      Result := Result + P^;
      Inc(P);
    end;

    while P^ in [' ', '='] do
      Inc(P);
  end;

var
  Line: string;
  P: PChar;
begin
  inherited Create;
  FStreamReader := TStreamReader.Create(Stream);

  while not FStreamReader.Eof do begin
    FStreamReader.ReadLine(Line);

    if Line.IsEmpty then
      Continue
    else if Line.StartsWith('#') then
    begin
      case Line.Substring(0, 3) of
        '#N ': FName := FName + Line.Substring(3);
        '#C ': FComment := FComment + Line.Substring(3);
      end;
    end
    else if Line.StartsWith('x') then
    begin
      P := @Line[1];

      while P^ <> #0 do
      begin
        case GetNextIdentifier(P) of
          'x': FWidth := ReadInteger(P);
          'y': FHeight := ReadInteger(P);
          'rule':
          begin
            while P^ = ' ' do
              Inc(P);

            while not (P^ in [#0, ' ', ',']) do
            begin
              FRule := FRule + P^;
              Inc(P);
            end;
          end;
        end;

        while P^ in [' ', ','] do
          Inc(P);
      end;

      Break;
    end;
  end;
end;

destructor TRleParser.Destroy;
begin
  FStreamReader.Free;
  inherited;
end;

procedure TRleParser.LoadInUniverse(const Universe: TUniverse; const AX, AY: Integer);
var
  Count: Integer;
  X, Y: Integer;
  Line: String;
  P: PChar;
  i: Integer;
begin
  X := AX;
  Y := AY;

  while not FStreamReader.Eof do
  begin
    FStreamReader.ReadLine(Line);
    P := @Line[1];

    while P^ <> #0 do
    begin
      Count := ReadInteger(P);

      if Count = 0 then
        Count := 1;

      case P^ of
        'b': Inc(X, Count);
        'o':
        begin
          for i := 1 to Count do
          begin
            Universe.SetBit(X, Y);
            Inc(X);
          end;
        end;
        '$':
        begin
          X := AX;
          Dec(Y, Count);
        end;
        '!': Break;
      end;

      Inc(P);
    end;
  end;
end;

end.

