unit uBigNum;

interface

uses
  Classess, SysUtils, uRandomGenerator;

type
  TDigit = Cardinal;
  PDigit = ^TDigit;
  PDigitArray = ^TDigitArray;
  TDigitArray = array [0..$1FFFFFFE] of TDigit;

  TBigNumCallback = function (State, Data, UserData: Integer): Boolean;

  TBigNum = class
  private
    FSign: Boolean;
    FDigit: PDigitArray;  { LSB in offset 0, MSB in offset FCount - 1 }
    FTotal: Integer;      { total number of digit available }
    FCount: Integer;      { actual digit used }

    function GetDigit(Index: Integer): TDigit;
    function GetBit(Index: Integer): Boolean;

    procedure Resize(Count: Integer);
    procedure Normalize;

    procedure AbsAddShifted(N: TBigNum; P: Integer);
    procedure AbsSubShifted(N: TBigNum; P: Integer);

    procedure AddShifted(N: TBigNum; P: Integer);
    procedure SubShifted(N: TBigNum; P: Integer);

    function AbsCompareShifted(N: TBigNum; P: Integer): Integer;
    function CompareShifted(N: TBigNum; P: Integer): Integer;

    function GetBitCount: Integer;
    procedure SetBitCount(Bits: Integer);

  public
    constructor Create; overload;
    constructor Create(N: TBigNum); overload;
    constructor CreateBytes(B: PChar; Length: Integer);
    constructor CreateBytesBE(B: PChar; Length: Integer);
    constructor CreateDigit(X: TDigit);

    destructor Destroy; override;

    procedure Swap(N: TBigNum);
    function Clone: TBigNum;

    property Sign: Boolean read FSign;
    property Count: Integer read FCount;
    property Digit[Index: Integer]: TDigit read GetDigit;
    property Bit[Index: Integer]: Boolean read GetBit;
    property BitCount: Integer read GetBitCount write SetBitCount;
    
    procedure Add(N: TDigit); overload;
    procedure Sub(N: TDigit); overload;
    procedure Mul(N: TDigit); overload;
    procedure DivMod(N: TDigit; R: TBigNum); overload;
    procedure Div_(N: TDigit); overload;
    procedure Mod_(N: TDigit); overload;
                                        
    procedure Add(N: TBigNum); overload;
    procedure Sub(N: TBigNum); overload;
    procedure Mul(N: TBigNum); overload;
    procedure DivMod(N, R: TBigNum); overload;
    procedure Div_(N: TBigNum); overload;
    procedure Mod_(N: TBigNum); overload;

    procedure PowMod(Exponent, Modulus: TBigNum);
    function ModInv(Modulus: TBigNum): Boolean;
                                                                                        
    procedure GenerateRandom(Bits: Integer);
    function RandomPrime(Bits: Integer; Iterations: Byte;
        Callback: TBigNumCallback; UserData: Integer): Boolean;

    procedure Shl_(N: Integer);
    procedure Shr_(N: Integer);

    procedure Gcd(N: TBigNum);

    procedure Neg;
    procedure Abs;

    procedure Copy(X: TBigNum);
    procedure CopyDigit(X: TDigit);

    procedure CopyBytes(B: PChar; Length: Integer);
    procedure CopyBytesBE(B: PChar; Length: Integer);
    
    procedure CopyHex(S: string);
    procedure CopyBase64(S: string);
    procedure CopyString(S: string);

    function ToHex: string;
    function ToBase64: string;
    function ToString: string; overload;
    function ToString(Radix: Integer): string; overload;
 
    procedure Clear;

    function AbsCompare(N: TBigNum): Integer;
    function Compare(N: TBigNum): Integer;

    function IsZero: Boolean;
    function IsOne: Boolean;
    function IsOdd: Boolean;
    function IsEven: Boolean;
    function IsNegative: Boolean;
    function IsPositive: Boolean;
    
    function IsPrimeSimple: Boolean;
    function IsPrimeFast(Iterations: Byte): Boolean;

 end;

implementation

const
  bin2asc: array [0..63] of Char =
    ( 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/' );

const
  DigitSize = SizeOf(TDigit);

var
  PrimesTable: PDigitArray;
  PrimesTableSize: Integer;
  asc2bin: array [0..255] of Byte;

procedure BuildPrimesTable;
const
  MaxBitMap = 2000;
var
  Map: PDigitArray;

  function GetBit(I: Integer): Boolean;
  var
    Offset: Integer;
    Mask: TDigit;
  begin
    Offset := I shr 5;
    Mask := 1 shl (I and 31);
    Result := (Map[Offset] and Mask) <> 0;
  end;

  procedure ClearBit(I: Integer);
  var
    Offset: Integer;
    Mask: TDigit;
  begin
    Offset := I shr 5;
    Mask := 1 shl (I and 31);
    Map[Offset] := Map[Offset] and (not Mask);
  end;

var
  I, J: Integer;

begin
  GetMem(Map, MaxBitMap shr 3);
  FillChar(Map^, MaxBitMap shr 3, -1);
  I := 2;
  PrimesTableSize := 0;
  while I < MaxBitMap do
  begin
    while (I < MaxBitMap) and (not GetBit(I)) do
      Inc(I);
    if I >= MaxBitMap then
      Break;

    Inc(PrimesTableSize);
      
    J := I + I;
    while J < MaxBitMap do
    begin
      ClearBit(J);
      Inc(J, I);
    end;
    Inc(I);
  end;

  GetMem(PrimesTable, PrimesTableSize * DigitSize);
  PrimesTableSize := 0;
  for I := 2 to MaxBitMap - 1 do
  begin
    if GetBit(I) then
    begin
      PrimesTable[PrimesTableSize] := I;
      Inc(PrimesTableSize);
    end;
  end;

  FreeMem(Map);
end;

procedure BuildAsc2BinTable;
var
  I: Integer;
begin
  FillChar(asc2bin, SizeOf(asc2bin), 255);
  for I := 0 to 63 do
    asc2bin[Ord(bin2asc[I])] := I;
end;

procedure SwapBytes(B: PChar; Length: Integer);
var
  R: PChar;
  Tmp: Char;
begin
  R := B;
  Inc(R, Length - 1);
  while R > B do
  begin
    Tmp := B^;
    B^ := R^;
    R^ := Tmp;
    Inc(B);
    Dec(R);
  end;
end;

{ === Begin of simple functions ============================== }
{ Put all short functions here for ease of debugging           }

function TBigNum.IsZero: Boolean;
begin
  Result := FCount = 0;
end;

function TBigNum.IsOne: Boolean;
begin
  Result := (FCount = 1) and (Digit[0] = 1) and (not FSign);
end;

function TBigNum.IsOdd: Boolean;
begin
  Result := (FCount > 0) and ((Digit[0] and 1) = 1);
end;

function TBigNum.IsEven: Boolean;
begin
  Result := not IsOdd;
end;

function TBigNum.IsNegative: Boolean;
begin
  Result := FSign;
end;

function TBigNum.IsPositive: Boolean;
begin
  Result := not FSign;
end;

{ === Begin of two-digit base operations ========================= }

function MulDigit(A, B: Cardinal): Int64; register;
asm
  mul edx
end;

function DivDigit(ALo, AHi, B: Cardinal): Cardinal; register;
asm
  div ecx
end;

{ === Begin of private methods =============================== }

procedure TBigNum.AbsAddShifted(N: TBigNum; P: Integer);
var
  Carry, D1, D2: TDigit;
  I, Max: Integer;
begin
  if IsZero then
  begin
    Copy(N);
    Shl_(P shl 5);
    Exit;
  end;

  if N.IsZero then
    Exit;

  { When adding two numbers with the same sign, maximum size needed is
    one digit larger than than current maximum size }
  if FCount < N.FCount + P then
    Max := N.FCount + P
  else
    Max := FCount;
  Resize(Max + 1);

  Carry := 0;
  for I := P to Max - 1 do
  begin
    D1 := Digit[I];
    D2 := N.Digit[I - P];
    FDigit[I] := D1 + D2 + Carry;
    if (FDigit[I] < D1) or (FDigit[I] < D2) then
      Carry := 1
    else
      Carry := 0;
  end;
  FDigit[Max] := Carry;
  Normalize;
end;

{ Precord Self >= N }
procedure TBigNum.AbsSubShifted(N: TBigNum; P: Integer);
var
  I, Max: Integer;
  Borrow, D1, D2: TDigit;
begin
  if IsZero then
  begin
    Copy(N);
    Shl_(P shl 5);
    Neg;
    Exit;
  end;

  if N.IsZero then
    Exit;

  if FCount < N.FCount + P then
    Resize(N.FCount + P);

  Borrow := 0;
  for I := P to FCount - 1 do
  begin
    D1 := Digit[I];
    D2 := N.Digit[I - P];
    FDigit[I] := D1 - D2 - Borrow;
    if D1 < D2 + Borrow then
      Borrow := 1
    else
      Borrow := 0;
  end;
  Normalize;
end;

procedure TBigNum.AddShifted(N: TBigNum; P: Integer);
var
  Diff: Integer;
  Tmp: TBigNum;
begin
  if FSign = N.FSign then
    AbsAddShifted(N, P)
  else
  begin
    Diff := AbsCompareShifted(N, P);
    if Diff = 0 then
      Resize(P)
    else if Diff > 0 then { Abs(Self) > Abs(N) }
      AbsSubShifted(N, P)
    else { Abs(Self) < Abs(N) }
    begin
      Tmp := N.Clone;
      Tmp.Shl_(P shl 5);
      Swap(Tmp);
      AbsSubShifted(Tmp, P);
      Tmp.Free;
    end;
  end;
end;

procedure TBigNum.SubShifted(N: TBigNum; P: Integer);
var
  Tmp: TBigNum;
begin
  Tmp := N.Clone;
  Tmp.Neg;
  AddShifted(Tmp, P);
  Tmp.Free;
end;

function TBigNum.AbsCompareShifted(N: TBigNum; P: Integer): Integer;
var
  I: Integer;
begin
  if FCount > N.FCount + P then
    Result := 1
  else if FCount < N.FCount + P then
    Result := -1
  else
  begin
    for I := FCount - 1 downto P do
    begin
      if Digit[I] > N.Digit[I - P] then
      begin
        Result := 1;
        Exit;
      end
      else if Digit[I] < N.Digit[I - P] then
      begin
        Result := -1;
        Exit;
      end;
    end;
    Result := 0;
  end;
end;

function TBigNum.CompareShifted(N: TBigNum; P: Integer): Integer;
begin
  if FSign and (not N.FSign) then
    Result := -1
  else if (not FSign) and N.FSign then
    Result := 1
  else
  begin
    Result := AbsCompareShifted(N, P);
    if FSign then
      Result := -Result;
  end;
end;

{ === Begin of public methods ================================ }

procedure TBigNum.Add(N: TDigit);
var
  T: TBigNum;
begin
  T := TBigNum.CreateDigit(N);
  Add(T);
  T.Free;
end;

procedure TBigNum.Sub(N: TDigit);
var
  T: TBigNum;
begin
  T := TBigNum.CreateDigit(N);
  Sub(T);
  T.Free;
end;

procedure TBigNum.Mul(N: TDigit);
var
  Carry: Cardinal;
  Long: Int64;
  I: Integer;
begin
  Resize(FCount + 1);
  Carry := 0;
  for I := 0 to FCount - 2 do
  begin
    Long := MulDigit(N, Digit[I]) + Carry;
    FDigit[I] := Long;
    Carry := Long shr 32;
  end;
  FDigit[FCount - 1] := Carry;
  Normalize;
end;

procedure TBigNum.DivMod(N: TDigit; R: TBigNum);
var
  Q: TBigNum;
  I: Integer;
begin
  Q := TBigNum.Create;
  Q.Resize(FCount);

  for I := Q.FCount - 1 downto 0 do
  begin
    Q.FDigit[I] := DivDigit(Digit[I], Digit[I + 1], N);
    Dec(FDigit[I], Q.Digit[I] * N);
    FDigit[I + 1] := 0;
  end;

  if R <> nil then
  begin
    FCount := 1;
    Normalize;
    Swap(R);
  end;

  Q.Normalize;
  Swap(Q);
  Q.Free;
end;

procedure TBigNum.Div_(N: TDigit);
begin
  DivMod(N, nil);
end;

procedure TBigNum.Mod_(N: TDigit);
var
  R: TBigNum;
begin
  R := TBigNum.Create;
  DivMod(N, R);
  Swap(R);
  R.Free;
end;

{ ==================================================== }

constructor TBigNum.Create;
begin
  FSign := False;
  FDigit := nil;
  FTotal := 0;
  FCount := 0;
  Resize(0);
end;

constructor TBigNum.Create(N: TBigNum);
begin     
  Create;
  Copy(N);
end;

constructor TBigNum.CreateBytes(B: PChar; Length: Integer);
begin
  Create;
  CopyBytes(B, Length);
end;

constructor TBigNum.CreateBytesBE(B: PChar; Length: Integer);
begin
  Create;
  CopyBytesBE(B, Length);
end;

constructor TBigNum.CreateDigit(X: TDigit);
begin
  Create;
  CopyDigit(X);
end;

destructor TBigNum.Destroy;
begin
  FreeMem(FDigit);
end;

function TBigNum.GetDigit(Index: Integer): Cardinal;
begin
  if Index < FCount then
    Result := FDigit[Index]
  else
    Result := 0;
end;

function TBigNum.GetBit(Index: Integer): Boolean;
var
  Mask: Cardinal;
begin
  Mask := 1 shl (Index and 31);
  Result := (Digit[Index shr 5] and Mask) <> 0;
end;

function TBigNum.Clone: TBigNum;
begin
  Result := TBigNum.Create(Self);
end;

procedure TBigNum.Add(N: TBigNum);
begin
  AddShifted(N, 0);
end;

procedure TBigNum.Sub(N: TBigNum);
begin
  SubShifted(N, 0);
end;

procedure TBigNum.Mul(N: TBigNum);
var
  Result: TBigNum;
  I, J: Integer;
  Carry: Cardinal;
  Long: Int64;
begin
  Result := TBigNum.Create;
  Result.FSign := FSign xor N.FSign;
  Result.Resize(FCount + N.FCount);
                  
  Carry := 0;
  for I := 0 to FCount - 1 do
  begin
    Long := MulDigit(N.Digit[0], Digit[I]) + Carry;
    Result.FDigit[I] := Long;
    Carry := Long shr 32;
  end;
  Result.FDigit[FCount] := Carry;

  for I := 1 to N.FCount - 1 do
  begin
    Carry := 0;
    for J := 0 to FCount - 1 do
    begin
      Long := MulDigit(N.Digit[I], Digit[J]) + Result.Digit[I + J] + Carry;
      Result.FDigit[I + J] := Long;
      Carry := Long shr 32;
    end;
    Result.FDigit[I + FCount] := Carry;
  end;

  Swap(Result);
  Result.Free;
  Normalize;
end;

procedure TBigNum.DivMod(N, R: TBigNum);
var
  Q, Tmp: TBigNum;
  T: TDigit;
  I, NormShift: Integer;
begin
  if N.IsZero then
    Exit;

  if IsZero then
  begin
    if R <> nil then
      R.Clear;
    Exit;
  end;

  if N.FCount = 1 then
  begin
    DivMod(N.Digit[0], R);
    if R <> nil then
      R.Sign := FSign;
    FSign := FSign xor N.FSign;
    Exit;
  end;

  I := AbsCompare(N);
  if I < 0 then
  begin
    if R <> nil then
      R.Copy(Self);
    Clear;
    Exit;
  end;
  if I = 0 then
  begin
    if R <> nil then
      R.Clear;
    Resize(1);
    FSign := FSign xor N.FSign;
    FDigit[0] := 1;
    Exit;
  end;

  Q := TBigNum.Create;
  Q.FSign := FSign xor N.FSign;
  Q.Resize(FCount - N.FCount + 1);

  { Normalize divisor }
  NormShift := (32 - (N.BitCount and 31)) and 31;
  N.Shl_(NormShift);
  Shl_(NormShift);

  { Find estimated divisor }
  T := N.Digit[N.FCount - 1];
  for I := 0 to N.FCount - 2 do
  begin
    if N.Digit[I] <> 0 then
    begin
      Inc(T);
      Break;
    end;
  end;

  Tmp := TBigNum.Create;
  for I := Q.FCount - 1 downto 0 do
  begin
    if T = 0 then
      Q.FDigit[I] := Digit[I + N.FCount]
    else
      Q.FDigit[I] := DivDigit(Digit[I + N.FCount - 1], Digit[I + N.FCount], T);

    Tmp.Copy(N);
    Tmp.Mul(Q.FDigit[I]);
    AbsSubShifted(Tmp, I);

    while AbsCompareShifted(N, I) >= 0 do
    begin
      Inc(Q.FDigit[I], 1);
      AbsSubShifted(N, I);
    end;
  end;
  Tmp.Free;

  { Denormalize remainder }
  Shr_(NormShift);

  if R <> nil then
    Swap(R);

  Q.Normalize;
  Swap(Q);  
  Q.Free;
end;

procedure TBigNum.Div_(N: TBigNum);
begin
  DivMod(N, nil);
end;

procedure TBigNum.Mod_(N: TBigNum);
var
  Tmp: TBigNum;
begin
  Tmp := TBigNum.Create;
  DivMod(N, Tmp);
  Swap(Tmp);
  Tmp.Free;
end;

procedure TBigNum.PowMod(Exponent, Modulus: TBigNum);
var
  Result: TBigNum;
  I: Integer;
  D, Mask: TDigit;
begin
  Result := TBigNum.CreateDigit(1);
  for I := Exponent.FCount - 1 downto 0 do
  begin
    D := Exponent.FDigit[I];
    Mask := $80000000;
    while Mask <> 0 do
    begin
      Result.Mul(Result);
      Result.Mod_(Modulus);
      if (D and Mask) <> 0 then
      begin
        Result.Mul(Self);
        Result.Mod_(Modulus);
      end;
      Mask := Mask shr 1;
    end;
  end;
  Swap(Result);
  Result.Free;
end;

procedure ExtendedEuclid(u, v, u1, u2, GCD: TBigNum);
var
  t1: TBigNum;
  t2: TBigNum;
  t3: TBigNum;
  zero: TBigNum;
  tmp: TBigNum;
  k: Integer;
begin
  tmp := TBigNum.Create;
  t1 := TBigNum.Create;
  t2 := TBigNum.Create;
  t3 := TBigNum.Create;
  zero := TBigNum.Create;

  try
    k := 0;
    while not u.GetBit(k) and not v.GetBit(k) do
      Inc(k);
    u.Shr_(k);
    v.Shr_(k);

    if u.Compare(v) < 0 then
      u.Swap(v);

    u1.CopyDigit(1);
    u2.CopyDigit(0);
    GCD.Copy(u);
    t1.Copy(v);
    t2.Copy(u);
    t2.Sub(1);
    t3.Copy(v);

    repeat
      repeat
        if GCD.IsEven then
        begin
          if u1.IsOdd or u2.IsOdd then
          begin
            u1.Add(v);
            u2.Add(u);
          end;
          u1.Shr_(1);
          u2.Shr_(1);
          GCD.Shr_(1);
        end;
        if t3.IsEven or (GCD.Compare(t3) < 0) then
        begin
          u1.Swap(t1);
          u2.Swap(t2);
          GCD.Swap(t3);
        end;
      until GCD.IsOdd;

      while (u1.Compare(t1) < 0) or (u2.Compare(t2) < 0) do
      begin
        u1.Add(v);
        u2.Add(u);
      end;
      u1.Sub(t1);
      u2.Sub(t2);
      GCD.Sub(t3);
    until t3.Compare(zero) <= 0;

    while (u1.Compare(v) >= 0) and (u2.Compare(u) >= 0) do
    begin
      u1.Sub(v);
      u2.Sub(u);
    end;
    u1.Shl_(k);
    u2.Shl_(k);
    GCD.Shl_(k);
  finally
    tmp.Free;
    t1.Free;
    t2.Free;
    t3.Free;
    zero.Free;
  end;
end;

function TBigNum.ModInv(Modulus: TBigNum): Boolean;
var
  A, B, X, Y, D: TBigNum;
begin
  A := TBigNum.Create;
  B := TBigNum.Create;
  D := TBigNum.Create;
  X := TBigNum.Create;
  Y := TBigNum.Create;
  try
    A.Copy(Self);
    B.Copy(Modulus);
    ExtendedEuclid(A, B, X, Y, D);
    Result := D.IsOne;
    if Result then
    begin
      Swap(A);
      Sub(Y);
    end;
  finally
    A.Free;
    B.Free;
    D.Free;
    X.Free;
    Y.Free;
  end;
end;

procedure TBigNum.Gcd(N: TBigNum);
var
  A, B, D, X, Y: TBigNum;
begin                 
  A := TBigNum.Create;
  B := TBigNum.Create;
  D := TBigNum.Create;
  X := TBigNum.Create;
  Y := TBigNum.Create;
  try
    ExtendedEuclid(Self, N, X, Y, D);
    Swap(D);
  finally
    A.Free;
    B.Free;
    D.Free;
    X.Free;
    Y.Free;
  end;
end;

procedure TBigNum.Neg;
begin
  if not IsZero then
    FSign := not FSign;
end;

procedure TBigNum.Abs;
begin
  FSign := False;
end;

procedure TBigNum.Copy(X: TBigNum);
begin
  FSign := X.FSign;
  Resize(X.FCount);
  Move(X.FDigit[0], FDigit[0], X.FCount * DigitSize);
end;

procedure TBigNum.CopyBytes(B: PChar; Length: Integer);
begin
  FSign := False;
  Resize((Length + DigitSize - 1) div DigitSize);
  FDigit[FCount - 1] := 0;
  Move(B^, FDigit[0], Length);
  Normalize;
end;

procedure TBigNum.CopyBytesBE(B: PChar; Length: Integer);
begin
  FSign := False;
  Resize((Length + DigitSize - 1) div DigitSize);
  FDigit[FCount - 1] := 0;
  Move(B^, FDigit[0], Length);
  SwapBytes(PChar(@FDigit[0]), Length);
  Normalize;
end;

procedure TBigNum.CopyDigit(X: TDigit);
begin
  Resize(1);
  FDigit[0] := X;
  Normalize;
end;

procedure TBigNum.CopyHex(S: string);
var
  I, Shift, Count: Integer;
  Digit: TDigit;
  C: Char;
begin
  Resize((((Length(S) + 1) div 2) + DigitSize - 1) div DigitSize);
  Shift := 0;
  Count := 0;
  Digit := 0;
  for I := Length(S) downto 1 do
  begin
    C := UpCase(S[I]);
    case C of
      '0'..'9': Digit := Digit or (TDigit(Ord(C) - Ord('0')) shl Shift);
      'A'..'F': Digit := Digit or (TDigit(Ord(C) - Ord('A') + 10) shl Shift);
    else
      Continue;
    end;
    Inc(Shift, 4);
    if Shift >= 32 then
    begin
      FDigit[Count] := Digit;
      Inc(Count);            
      Digit := 0;
      Shift := 0;
    end;
  end;
  if Shift > 0 then
    FDigit[Count] := Digit;
  FCount := Count + 1;
  Normalize;
end;

procedure TBigNum.CopyBase64(S: string);
var
  A, B: Byte;
  I, O: Integer;
begin
  Resize(((Length(S) * 3 div 4) + DigitSize - 1) div DigitSize);
  FDigit[FCount - 1] := 0;
  
  I := 1;
  O := 0;
  while I <= Length(S) - 3 do
  begin
    A := asc2bin[Ord(S[I])];
    B := asc2bin[Ord(S[I + 1])];
    if (A = 255) or (B = 255) then
      Break;
    PByteArray(FDigit)[O] := (A shl 2) or (B shr 4);
    Inc(O);
            
    A := asc2bin[Ord(S[I + 2])];
    if (A = 255) then
      Break;
    PByteArray(FDigit)[O] := (B shl 4) or (A shr 2);
    Inc(O);

    B := asc2bin[Ord(S[I + 3])];
    if (B = 255) then
      Break;
    PByteArray(FDigit)[O] := (A shl 6) or (B shr 0);
    Inc(O);

    Inc(I, 4);
  end;
  FCount := (O + DigitSize - 1) div DigitSize;
  Normalize;
end;

procedure TBigNum.CopyString(S: string);
var
  I: Integer;
begin
  Clear;
  for I := 1 to Length(S) do
  begin
    Mul(10);
    Add(Ord(S[I]) - Ord('0'));
  end;
end;

function TBigNum.ToHex: string;
var
  I: Integer;
begin
  if FCount = 0 then
    Result := '0'
  else
  begin
    Result := '';
    for I := FCount - 1 downto 0 do
      Result := Result + IntToHex(Digit[I], 8);
  end;
  for I := 1 to Length(Result) do
  begin
    if (Result[I] <> '0') or (I = Length(Result)) then
    begin
      Result := System.Copy(Result, I, Length(Result));
      Break;
    end;
  end;
end;

function TBigNum.ToBase64: string;
var
  I, Count: Integer;
  P: PByteArray;
  A, B, C: Byte;
begin
  Result := '';

  Count := FCount * DigitSize;
  P := PByteArray(FDigit);
  while (Count > 0) and (P[Count - 1] = 0) do
    Dec(Count);
               
  I := 0;
  while Count - I >= 3 do
  begin
    A := P[I];
    B := P[I + 1];
    C := P[I + 2];
    Result := Result + bin2asc[A shr  2];
    Result := Result + bin2asc[((A shl 4) or (B shr 4)) and 63];
    Result := Result + bin2asc[((B shl 2) or (C shr 6)) and 63];
    Result := Result + bin2asc[C and 63];
    Inc(I, 3);
  end;
  if Count - I = 2 then
  begin
    A := P[I];
    B := P[I + 1];
    Result := Result + bin2asc[A shr  2];
    Result := Result + bin2asc[((A shl 4) or (B shr 4)) and 63];
    Result := Result + bin2asc[(B shl 2) and 63];
    Result := Result + '=';
  end
  else if Count - I = 1 then
  begin
    A := P[I];
    Result := Result + bin2asc[A shr  2];
    Result := Result + bin2asc[(A shl 4) and 63];
    Result := Result + '==';
  end;
end;

function TBigNum.ToString: string;
begin
  Result := ToString(10);
end;

function TBigNum.ToString(Radix: Integer): string;
var
  N, R: TBigNum;
begin
  if Radix = 16 then
  begin
    Result := ToHex;
    Exit;
  end;

  Result := '';

  N := Clone;
  N.FSign := False;
  R := TBigNum.Create;
  while not N.IsZero do
  begin
    N.DivMod(Radix, R);
    if R.IsZero then
      Result := '0' + Result
    else if R.Digit[0] < 10 then
      Result := Char(Ord('0') + R.Digit[0]) + Result
    else
      Result := Char(Ord('A') + R.Digit[0] - 10) + Result;
  end;
  N.Free;
  R.Free;
  if Result = '' then
    Result := '0';
  if FSign then
    Result := '-' + Result;
end;

procedure TBigNum.Clear;
begin
  Resize(0);
end;
    
procedure TBigNum.Resize(Count: Integer);
const
  Granularity = 8;
var
  Total: Integer;
begin
  if Count < 0 then
    Count := 0;
    
  Total := (Count + Granularity) and (not (Granularity - 1));
  if FTotal <> Total then
  begin
    ReallocMem(FDigit, Total * DigitSize);
    FTotal := Total;
  end;              
  FillChar(FDigit[FCount], (FTotal - FCount) * DigitSize, 0);
  FCount := Count;
end;

procedure TBigNum.Normalize;
var
  I: Integer;
  Size: Integer;
begin
  Size := 0;
  for I := FCount - 1 downto 0 do
  begin
    if FDigit[I] <> 0 then
    begin
      Size := I + 1;
      Break;
    end;
  end;
  Resize(Size);

  { Normalize zero's sign (zero is positive) }
  if FCount = 0 then
    FSign := False;
end;

function TBigNum.AbsCompare(N: TBigNum): Integer;
begin
  Result := AbsCompareShifted(N, 0);
end;

function TBigNum.Compare(N: TBigNum): Integer;
begin
  Result := CompareShifted(N, 0);
end;

function TBigNum.IsPrimeSimple: Boolean;
var
  I: Integer;
  D, R: TBigNum;
begin
  D := TBigNum.Create;
  R := TBigNum.Create;
  Result := True;
  for I := 0 to PrimesTableSize - 1 do
  begin
    D.Copy(Self);
    R.CopyDigit(PrimesTable[I]);
    if D.Compare(R) = 0 then
      Break;

    D.DivMod(R, R);
    if D.IsZero then
    begin
      Result := False;
      Break;
    end;
  end;
  D.Free;
  R.Free;
end;

{ Rabbin-Miller }
function TBigNum.IsPrimeFast(Iterations: Byte): Boolean;
var
  w : TBigNum;
  w1 : TBigNum;
  m : TBigNum;
  b : TBigNum;
  z : TBigNum;
  a : Integer;
  j : Integer;
  i : Integer;
  one : TBigNum;
  random : TRandomGenerator;
  Buf : PChar;
  len : Integer;
  Mask: TDigit;
begin
  Result := False;
  if not GetBit(0) then
    Exit;

  len := ((FCount shr 1) + 1) * DigitSize;
  random := TRandomGenerator.Create;
  GetMem(Buf, len);

  w := Self;
  w1 := TBigNum.Create;
  m := TBigNum.Create;
  b := TBigNum.Create;
  z := TBigNum.Create;
  one := TBigNum.CreateDigit(1);

  Mask := $FFFFFFFF shr (32 - (w.BitCount and 31));

  try
    { find w = 1 + (2^a) * m }
    {   where m is odd and 2^a is the largest power of 2 dividing w - 1 }
    w1.Copy(w);
    w1.Sub(1);
    if w1.IsZero then
      Exit;

    m.Copy(w1);

    a := 1;
    while not m.GetBit(a) do
      Inc(a);
    m.Shr_(a);

    for i := 1 to Iterations do
    begin
      { generate random number b: 1 < b < w }
      repeat
        random.RandomBytes(Buf^, len);
        b.CopyBytes(Buf, len);
        b.FDigit[b.FCount - 1] := b.Digit[b.FCount - 1] and Mask;
      until (b.Compare(one) > 0) and (b.Compare(w) < 0);

      { z = b^m mod w }
      z.Copy(b);
      z.PowMod(m, w);

      if z.IsOne or (z.Compare(w1) = 0) then
        Continue;

      j := 0;
      while True do
      begin
        Inc(j);
        if z.IsOne then
          Exit;
        if z.Compare(w1) = 0 then
          Break;
        if j >= a then
          Exit;
        z.Mul(z);
        z.Mod_(w);
      end;
    end;
    Result := True;

  finally
    FreeMem(Buf, len);
    w1.Free;
    m.Free;
    b.Free;
    z.Free;
    one.Free;
    random.Free;
  end;
end;

procedure TBigNum.GenerateRandom(Bits: Integer);
var
  RG: TRandomGenerator;
  Mask: TDigit;
begin
  RG := TRandomGenerator.Create;

  FSign := False;
  Resize((Bits + 31) shr 5);

  RG.RandomBytes(FDigit[0], FCount * DigitSize);

  Mask := $FFFFFFFF shr (32 - (Bits and 31));
  FDigit[FCount - 1] := Digit[FCount - 1] and Mask;
  Mask := $80000000 shr (32 - (Bits and 31));
  FDigit[FCount - 1] := Digit[FCount - 1] or Mask;

  RG.Free;
end;

function TBigNum.RandomPrime(Bits: Integer; Iterations: Byte;
    Callback: TBigNumCallback; UserData: Integer): Boolean;
var
  Data: Integer;
begin
  Data := 0;
  while True do
  begin
    GenerateRandom(Bits);
    FDigit[0] := Digit[0] or 1;

    while True do
    begin
      if IsPrimeSimple then
        if IsPrimeFast(Iterations) then
        begin
          Result := True;
          Exit;
        end;

      Inc(Data);
      if Assigned(Callback) then
        if not Callback(0, Data, UserData) then
        begin
          Result := False;
          Exit;
        end;

      Add(2);
    end;
  end;
end;

procedure TBigNum.Swap(N: TBigNum);
var
  TmpSign: Boolean;
  TmpDigit: PDigitArray;
  TmpTotal: Integer;
  TmpCount: Integer;
begin
  TmpSign := FSign;
  TmpDigit := FDigit;
  TmpTotal := FTotal;
  TmpCount := FCount;

  FSign := N.FSign;
  FDigit := N.FDigit;
  FTotal := N.FTotal;
  FCount := N.FCount;

  N.FSign := TmpSign;
  N.FDigit := TmpDigit;
  N.FTotal := TmpTotal;
  N.FCount := TmpCount;
end;

procedure TBigNum.Shl_(N: Integer);
var
  Delta, I: Integer;
begin
  if FCount < 0 then
    Shr_(-FCount)
  else
  begin
    Delta := N shr 5;
    N := N and 31;
    Resize(FCount + Delta + 1);
    if Delta > 0 then
    begin
      Move(FDigit[0], FDigit[Delta], (FCount - Delta) * DigitSize);
      FillChar(FDigit[0], Delta * DigitSize, 0);
    end;
    if N > 0 then
    begin
      FDigit[FCount - 1] := 0;
      for I := FCount - 1 downto 1 do
        FDigit[I] := (Digit[I] shl N) or (Digit[I - 1] shr (32 - N));
      FDigit[0] := Digit[0] shl N;
    end;
    Normalize;
  end;
end;

procedure TBigNum.Shr_(N: Integer);
var
  Delta, I: Integer;
begin
  if FCount < 0 then
    Shl_(-FCount)
  else
  begin
    Delta := N shr 5;
    N := N and 31;
    if Delta > 0 then
      Move(FDigit[Delta], FDigit[0], (FCount - Delta) * DigitSize);
    Resize(FCount - Delta);
    FDigit[FCount] := 0;
    if N > 0 then
    begin
      for I := 0 to FCount - 1 do
        FDigit[I] := (Digit[I] shr N) or (Digit[I + 1] shl (32 - N));
    end;
    Normalize;
  end;
end;

function TBigNum.GetBitCount: Integer;
var
  Mask: Cardinal;
begin
  Result := FCount * 32;
  Mask := $80000000;
  while ((Digit[FCount - 1] and Mask) = 0) and (Mask <> 0) do
  begin
    Dec(Result);
    Mask := Mask shr 1;
  end;
end;

procedure TBigNum.SetBitCount(Bits: Integer);
var
  Mask: Cardinal;
begin
  if Bits < GetBitCount then
  begin
    Resize((Bits + 31) shr 5);
    Mask := $FFFFFFFF shr ((32 - Bits) and 31);
    FDigit[FCount - 1] := FDigit[FCount - 1] and Mask;
    Normalize;
  end;
end;

initialization
  BuildPrimesTable;
  BuildAsc2BinTable;

finalization
  FreeMem(PrimesTable);

end.
