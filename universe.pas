unit Universe;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Node;

type

  { TUniverse }

  TUniverse = class
  private
    FGenerationCount: UInt64;
    FRoot: INode;
  public
    constructor Create;
    procedure Clear;
    procedure SetBit(const x, y: Integer);
    procedure RunStep;
    property Root: INode read FRoot;
  end;

implementation

{ TUniverse }

constructor TUniverse.Create;
begin
  inherited;
  FRoot := TNode.Create;
end;

procedure TUniverse.Clear;
begin
  FRoot := TNode.Create;
end;

procedure TUniverse.SetBit(const x, y: Integer);
var
  MaxCoordinate: Integer;
begin
  while (True) do
  begin
    MaxCoordinate := 1 shl (FRoot.Level - 1);

    if ((-MaxCoordinate <= x) and (x < MaxCoordinate - 1)
    and (-MaxCoordinate <= y) and (y < MaxCoordinate - 1))
    then
      Break;

    FRoot := FRoot.ExpandUniverse;
  end;

  FRoot := FRoot.SetBit(x, y);
end;

procedure TUniverse.RunStep;
begin
  while ((FRoot.Level < 3)
     or (FRoot.NW.Population <> FRoot.NW.SE.SE.Population)
     or (FRoot.NE.Population <> FRoot.NE.SW.SW.Population)
     or (FRoot.SW.Population <> FRoot.SW.NE.NE.Population)
     or (FRoot.SE.Population <> FRoot.SE.NW.NW.Population))
  do
    FRoot := FRoot.ExpandUniverse;

  FRoot := FRoot.NextGeneration;
  Inc(FGenerationCount);
end;

end.

