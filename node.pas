unit Node;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}

interface

uses
  LGHashMap;

type

  { INode }

  INode = interface
  ['{A8ECD00D-5DEB-44EE-990A-DD3036081382}']
    function NextGeneration: INode;
    function CenteredSubnode: INode;
    function CenteredSubSubNode: INode;
    function GetNW: INode;
    function GetNE: INode;
    function GetSW: INode;
    function GetSE: INode;
    function GetLevel: Cardinal;
    function GetPopulation: Cardinal;
    function GetAlive: Boolean;
    function GetBit(const X, Y: Integer): Integer;
    function SetBit(const X, Y: Integer): INode;
    function EmptyTree(const Level: Cardinal): INode;
    function ExpandUniverse: INode;
    function GetLastUsed: TDateTime;
    procedure SetLastUsed(const Value: TDateTime);
    property NW: INode read GetNW;
    property NE: INode read GetNE;
    property SW: INode read GetSW;
    property SE: INode read GetSE;
    property Level: Cardinal read GetLevel;
    property Population: Cardinal read GetPopulation;
    property Alive: Boolean read GetAlive;
    property LastUsed: TDateTime read GetLastUsed write SetLastUsed;
  end;

  { TNode }

  TNode = class(TInterfacedObject, INode)
  private
  type
    TNodeHashMap = specialize TGBaseHashMapQP<INode, INode, TNode>;
  class var
    FNodeHashMap: TNodeHashMap;
    class constructor Create;
    class destructor Destroy;
  var
    FNW, FNE: INode;
    FSW, FSE: INode;
    FLevel: Cardinal;
    FPopulation: Cardinal;
    FAlive: Boolean;
    FResult: INode;
    FLastUsed: TDateTime;
    {%H-}constructor Init(const Alive: Boolean);
    {%H-}constructor Init(const NW, NE, SW, SE: INode);
    function GetNW: INode;
    function GetNE: INode;
    function GetSW: INode;
    function GetSE: INode;
    function GetLevel: Cardinal;
    function GetPopulation: Cardinal;
    function GetAlive: Boolean;
    class function OneGen(Bitmask: UInt16): INode;
    function SlowSimulation: INode;
    function EmptyTree(const Level: Cardinal): INode;
    function ExpandUniverse: INode;
    function GetLastUsed: TDateTime;
    procedure SetLastUsed(const Value: TDateTime);
    class function HashCode(constref Node: INode): SizeInt;
    class function Equal(constref L, R: INode): Boolean;
  public
    class function Create: INode;
    class function Create(const Alive: Boolean): INode;
    class function Create(const NW, NE, SW, SE: INode): INode;
    function NextGeneration: INode;
    function CenteredSubnode: INode;
    function CenteredSubSubNode: INode;
    class function CenteredHorizontalSubnode(const W, E: INode): INode;
    class function CenteredVerticalSubnode(const N, S: INode): INode;
    function GetBit(const X, Y: Integer): Integer;
    function SetBit(const X, Y: Integer): INode;
    class procedure GarbageCollector;
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  LGHash;

class constructor TNode.Create;
begin
  FNodeHashMap := TNodeHashMap.Create;
end;

class destructor TNode.Destroy;
begin
  FNodeHashMap.Free;
end;

function TNode.GetNW: INode;
begin
  Result := FNW;
end;

function TNode.GetNE: INode;
begin
  Result := FNE;
end;

function TNode.GetSW: INode;
begin
  Result := FSW;
end;

function TNode.GetSE: INode;
begin
  Result := FSE;
end;

function TNode.GetLevel: Cardinal;
begin
  Result := FLevel;
end;

function TNode.GetPopulation: Cardinal;
begin
  Result := FPopulation;
end;

function TNode.GetAlive: Boolean;
begin
  Result := FAlive;
end;

class function TNode.OneGen(Bitmask: UInt16): INode;
var
  Alive: Integer;
  NeighborCount: Integer;
begin
  if Bitmask = 0 then
    Exit(TNode.Create(False));

  Alive := (Bitmask shr 5) and 1;
  Bitmask := Bitmask and $757;
  NeighborCount := 0;

  while Bitmask <> 0 do
  begin
    Inc(NeighborCount);
    Bitmask := Bitmask and (Bitmask - 1);
  end;

  Result := TNode.Create((NeighborCount = 3) or ((Alive = 1) and (NeighborCount = 2)));
end;

function TNode.SlowSimulation: INode;
var
  x, y: Integer;
  AllBits: UInt16;
begin
  AllBits := 0;

  for y := -2 to 1 do
  begin
    for x := -2 to 1 do
    begin
      AllBits := (AllBits shl 1) or GetBit(x, y);
    end;
  end;

  Result := Tnode.Create(
    TNode.OneGen(AllBits shr 5), TNode.OneGen(AllBits shr 4),
    TNode.OneGen(AllBits shr 1), TNode.OneGen(AllBits)
  );
end;

function TNode.EmptyTree(const Level: Cardinal): INode;
var
  Node: INode;
begin
  if Level = 0 then
     Exit(TNode.Create(False));

  Node := EmptyTree(Level - 1);
  Result := TNode.Create(
    Node, Node,
    Node, Node
  );
end;

function TNode.ExpandUniverse: INode;
var
  Border: INode;
begin
  Border := EmptyTree(FLevel - 1);
  Result := TNode.Create(
    Tnode.Create(
      Border, Border,
      Border, FNW
    ),
    Tnode.Create(
      Border, Border,
      FNE,    Border
    ),
    Tnode.Create(
      Border, FSW,
      Border, Border
    ),
    Tnode.Create(
      FSE,    Border,
      Border, Border
    )
  );
end;

function TNode.GetLastUsed: TDateTime;
begin
  Result := FLastUsed;
end;

procedure TNode.SetLastUsed(const Value: TDateTime);
begin
  FLastUsed := Value;
end;

function TNode.GetBit(const X, Y: Integer): Integer;
var
  Offset: Integer;
begin
  if FLevel = 0 then
    Exit(IfThen(FAlive, 1));

  Offset := (1 shl FLevel) shr 2;

  if X < 0 then
  begin
    if Y < 0 then
      Result := FNW.GetBit(X + Offset, Y + Offset)
    else
      Result := FSW.GetBit(X + Offset, Y - Offset);
  end
  else
  begin
    if Y < 0 then
      Result := FNE.GetBit(X - Offset, Y + Offset)
    else
      Result := FSE.GetBit(X - Offset, Y - Offset);
  end;
end;

function TNode.SetBit(const X, Y: Integer): INode;
var
  Offset: Integer;
begin
  if FLevel = 0 then
     Exit(TNode.Create(True));

  Offset := (1 shl FLevel) shr 2;

  if X < 0 then
  begin
    if Y < 0 then
      Result := TNode.Create(
        FNW.SetBit(X + Offset, Y + Offset), FNE,
        FSW,                                FSE
      )
    else
      Result := TNode.Create(
        FNW,                                FNE,
        FSW.SetBit(X + Offset, Y - Offset), FSE
      );
  end
  else
  begin
    if Y < 0 then
      Result := TNode.Create(
        FNW, FNE.SetBit(X - Offset, Y + Offset),
        FSW, FSE
      )
    else
      Result := TNode.Create(
        FNW, FNE,
        FSW, FSE.SetBit(X - Offset, Y - Offset)
      );
  end;
end;

class procedure TNode.GarbageCollector;

  function TestNode(const Node: INode): Boolean;
  begin
    Result := ((Node as TNode).RefCount < 3) and (SecondsBetween(Now, Node.LastUsed) > 30);
  end;

begin
  FNodeHashMap.RemoveIf(@TestNode);
end;

constructor TNode.Init(const Alive: Boolean);
begin
  inherited Create;
  FAlive := Alive;
  FPopulation := IfThen(FAlive, 1);
  FLastUsed := Now;
end;

constructor TNode.Init(const NW, NE, SW, SE: INode);
begin
  inherited Create;
  FNW := NW;
  FNE := NE;
  FSW := SW;
  FSE := SE;
  FLevel := FNW.Level + 1;
  FPopulation := FNW.Population + FNE.Population + FSW.Population + FSE.Population;
  FLastUsed := Now;
end;

class function TNode.Create: INode;
begin
  Result := TNode.Create(False).EmptyTree(3);
  FNodeHashMap.Clear;
end;

class function TNode.Create(const Alive: Boolean): INode;
begin
  Result := Init(Alive);

  if not FNodeHashMap.Add(Result, Result) then
  begin
    Result := FNodeHashMap[Result];
    Result.LastUsed := Now;
  end;
end;

class function TNode.Create(const NW, NE, SW, SE: INode): INode;
begin
  Result := Init(NW, NE, SW, SE);

  if not FNodeHashMap.Add(Result, Result) then
  begin
    Result := FNodeHashMap[Result];
    Result.LastUsed := Now;
  end;
end;

function TNode.NextGeneration: INode;
var
  N00, N01, N02: INode;
  N10, N11, N12: INode;
  N20, N21, N22: INode;
begin
  if Assigned(FResult) then
    Exit(FResult);

  if FPopulation = 0 then
    Result := FNW
  else if FLevel = 2 then
    Result := SlowSimulation
  else
  begin
    N00 := FNW.CenteredSubnode;
    N01 := CenteredHorizontalSubnode(FNW, FNE);
    N02 := FNE.CenteredSubnode;
    N10 := CenteredVerticalSubnode(FNW, FSW);
    N11 := CenteredSubSubNode;
    N12 := CenteredVerticalSubnode(FNE, FSE);
    N20 := FSW.CenteredSubnode;
    N21 := CenteredHorizontalSubnode(FSW, FSE);
    N22 := FSE.CenteredSubnode;

    Result := TNode.Create(
      TNode.Create(
        N00, N01,
        N10, N11
      ).NextGeneration,
      TNode.Create(
        N01, N02,
        N11, N12
      ).NextGeneration,
      TNode.Create(
        N10, N11,
        N20, N21
      ).NextGeneration,
      TNode.Create(
        N11, N12,
        N21, N22
      ).NextGeneration
    );
  end;

  FResult := Result;
end;

function TNode.CenteredSubnode: INode;
begin
  Result := TNode.Create(
    FNW.SE, FNE.SW,
    FSW.NE, FSE.NW
  );
end;

function TNode.CenteredSubSubNode: INode;
begin
  Result := TNode.Create(
    FNW.SE.SE, FNE.SW.SW,
    FSW.NE.NE, FSE.NW.NW
  );
end;

class function TNode.CenteredHorizontalSubnode(const W, E: INode): INode;
begin
  Result := TNode.Create(
    W.NE.SE, E.NW.SW,
    W.SE.NE, E.SW.NW
  );
end;

class function TNode.CenteredVerticalSubnode(const N, S: INode): INode;
begin
  Result := TNode.Create(
    N.SW.SE, N.SE.SW,
    S.NW.NE, S.NE.NW
  );
end;

class function TNode.HashCode(constref Node: INode): SizeInt;
begin
  if Node.Level = 0 then
    Exit(Node.Population);

  {$ifdef CPU64}
  Result := TxxHash32LE.HashQWord(QWord(Node.NW));
  Result := TxxHash32LE.HashQWord(QWord(Node.NE), Result);
  Result := TxxHash32LE.HashQWord(QWord(Node.SW), Result);
  Result := TxxHash32LE.HashQWord(QWord(Node.SE), Result);
  {$else}
  Result := TxxHash32LE.HashDWord(DWord(Node.NW));
  Result := TxxHash32LE.HashDWord(DWord(Node.NE), Result);
  Result := TxxHash32LE.HashDWord(DWord(Node.SW), Result);
  Result := TxxHash32LE.HashDWord(DWord(Node.SE), Result);
  {$endif}
end;

class function TNode.Equal(constref L, R: INode): Boolean;
begin
  if L.Level <> R.Level then
    Exit(False);

  if L.Level = 0 then
    Result := (L.Alive = R.Alive)
  else
    Result := (L.NW = R.NW)
          and (L.NE = R.NE)
          and (L.SW = R.SW)
          and (L.SE = R.SE);
end;

end.

