unit uHash;

interface

uses
  SysUtils;

type
  TDigest = class
  private
    FBuffer: PChar;
    FLength: Integer;
    function GetBytes(Index: Integer): Byte;
  public
    constructor Create(Buffer: PChar; Length: Integer);
    function ToString: string;
    property Bytes[Index: Integer]: Byte read GetBytes; default;
    property Buffer: PChar read FBuffer;
    property Length: Integer read FLength;
  end;

type
  THash = class
  protected
    function GetDigest: TDigest; virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure Update(const Buffer; Length: Integer); virtual; abstract;
    procedure Finalize; virtual; abstract;
    function Hash(const Buffer; Length: Integer): TDigest;
    property Digest: TDigest read GetDigest;
  end;

type
  TMD5 = class(THash)
  private           
    FRegs: array [0..3] of Cardinal;
    FBuffer: array [0..63] of Byte;
    FLength: Integer;
    FTotal: Integer;
    FDigest: TDigest;
    procedure Transform;
  protected
    function GetDigest: TDigest; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; override;
    procedure Update(const Buffer; Length: Integer); override;
    procedure Finalize; override;
  end;

type
  TSHA1 = class(THash)  
  private           
    FRegs: array [0..4] of Cardinal;
    FBuffer: array [0..63] of Byte;
    FLength: Integer;
    FTotal: Integer;
    FDigest: TDigest;
    procedure Transform;
  protected
    function GetDigest: TDigest; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; override;
    procedure Update(const Buffer; Length: Integer); override;
    procedure Finalize; override;
  end;

implementation

{ === Utility Functions ====================================== }

function Rol(I, C: Cardinal): Cardinal; register;
asm
  mov ecx, edx
  rol eax, cl
end;

function SwapBytes(C: Cardinal): Cardinal; overload;
begin
  Result := (C shl 24) or ((C and $FF00) shl 8) or ((C shr 8) and $FF00) or (C shr 24);
end;

function SwapBytes(I: Int64): Int64; overload;
begin
  Result := (Int64(SwapBytes(Cardinal(I))) shl 32) or SwapBytes(Cardinal(I shr 32));
end;

{ === TDigest ================================================ }

constructor TDigest.Create(Buffer: PChar; Length: Integer);
begin
  FBuffer := Buffer;
  FLength := Length;
end;

function TDigest.GetBytes(Index: Integer): Byte;
begin
  Result := PByteArray(FBuffer)[Index];
end;

function TDigest.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FLength do
    Result := Result + IntToHex(Bytes[I], 2);
end;

{ === THash ================================================== }

function THash.Hash(const Buffer; Length: Integer): TDigest;
begin
  Reset;
  Update(Buffer, Length);
  Finalize;
  Result := Digest;
end;

{ === TMD5 =================================================== }

constructor TMD5.Create;
begin
  FDigest := TDigest.Create(@FRegs[0], SizeOf(FRegs));
  Reset;
end;

destructor TMD5.Destroy;
begin
  FDigest.Free;
  inherited;
end;

procedure TMD5.Reset;
begin
  FLength := 0;
  FTotal := 0;
  FRegs[0] := $67452301;
  FRegs[1] := $EFCDAB89;
  FRegs[2] := $98BADCFE;
  FRegs[3] := $10325476;
end;

procedure TMD5.Transform;
type
  TCardinalArray = array [0..15] of Cardinal;
  PCardinalArray = ^TCardinalArray;
const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;
var
  A: Cardinal;
  B: Cardinal;
  C: Cardinal;
  D: Cardinal;
  P: PCardinalArray;

  procedure FF(var A: Cardinal;  B, C, D, X, S, AC: Cardinal);
  begin
    A := Rol(A + ((B and C) or (not B and D)) + X + AC, S) + B;
  end;

  procedure GG(var A: Cardinal;  B, C, D, X, S, AC: Cardinal);
  begin
    A := Rol(A + ((B and D) or (C and not D)) + X + AC, S) + B;
  end;

  procedure HH(var A: Cardinal;  B, C, D, X, S, AC: Cardinal);
  begin
    A := Rol(A + (B xor C xor D) + X + AC, S) + B;
  end;

  procedure II(var A: Cardinal;  B, C, D, X, S, AC: Cardinal);
  begin
    A := Rol(A + (C xor (B or not D)) + X + AC, S) + B;
  end;

begin
  P := PCardinalArray(@FBuffer[0]);
  A := FRegs[0];
  B := FRegs[1];
  C := FRegs[2];
  D := FRegs[3];

  {round 1}
  FF(A, B, C, D, P[ 0], S11, $D76AA478);  { 1 }
  FF(D, A, B, C, P[ 1], S12, $E8C7B756);  { 2 }
  FF(C, D, A, B, P[ 2], S13, $242070DB);  { 3 }
  FF(B, C, D, A, P[ 3], S14, $C1BDCEEE);  { 4 }
  FF(A, B, C, D, P[ 4], S11, $F57C0FAF);  { 5 }
  FF(D, A, B, C, P[ 5], S12, $4787C62A);  { 6 }
  FF(C, D, A, B, P[ 6], S13, $A8304613);  { 7 }
  FF(B, C, D, A, P[ 7], S14, $FD469501);  { 8 }
  FF(A, B, C, D, P[ 8], S11, $698098D8);  { 9 }
  FF(D, A, B, C, P[ 9], S12, $8B44F7AF);  { 10 }
  FF(C, D, A, B, P[10], S13, $FFFF5BB1);  { 11 }
  FF(B, C, D, A, P[11], S14, $895CD7BE);  { 12 }
  FF(A, B, C, D, P[12], S11, $6B901122);  { 13 }
  FF(D, A, B, C, P[13], S12, $FD987193);  { 14 }
  FF(C, D, A, B, P[14], S13, $A679438E);  { 15 }
  FF(B, C, D, A, P[15], S14, $49B40821);  { 16 }

  {round 2}
  GG(A, B, C, D, P[ 1], S21, $F61E2562);  { 17 }
  GG(D, A, B, C, P[ 6], S22, $C040B340);  { 18 }
  GG(C, D, A, B, P[11], S23, $265E5A51);  { 19 }
  GG(B, C, D, A, P[ 0], S24, $E9B6C7AA);  { 20 }
  GG(A, B, C, D, P[ 5], S21, $D62F105D);  { 21 }
  GG(D, A, B, C, P[10], S22, $02441453);  { 22 }
  GG(C, D, A, B, P[15], S23, $D8A1E681);  { 23 }
  GG(B, C, D, A, P[ 4], S24, $E7D3FBC8);  { 24 }
  GG(A, B, C, D, P[ 9], S21, $21E1CDE6);  { 25 }
  GG(D, A, B, C, P[14], S22, $C33707D6);  { 26 }
  GG(C, D, A, B, P[ 3], S23, $F4D50D87);  { 27 }
  GG(B, C, D, A, P[ 8], S24, $455A14ED);  { 28 }
  GG(A, B, C, D, P[13], S21, $A9E3E905);  { 29 }
  GG(D, A, B, C, P[ 2], S22, $FCEFA3F8);  { 30 }
  GG(C, D, A, B, P[ 7], S23, $676F02D9);  { 31 }
  GG(B, C, D, A, P[12], S24, $8D2A4C8A);  { 32 }

  {round 3}
  HH(A, B, C, D, P[ 5], S31, $FFFA3942);  { 33 }
  HH(D, A, B, C, P[ 8], S32, $8771F681);  { 34 }
  HH(C, D, A, B, P[11], S33, $6D9D6122);  { 35 }
  HH(B, C, D, A, P[14], S34, $FDE5380C);  { 36 }
  HH(A, B, C, D, P[ 1], S31, $A4BEEA44);  { 37 }
  HH(D, A, B, C, P[ 4], S32, $4BDECFA9);  { 38 }
  HH(C, D, A, B, P[ 7], S33, $F6BB4B60);  { 39 }
  HH(B, C, D, A, P[10], S34, $BEBFBC70);  { 40 }
  HH(A, B, C, D, P[13], S31, $289B7EC6);  { 41 }
  HH(D, A, B, C, P[ 0], S32, $EAA127FA);  { 42 }
  HH(C, D, A, B, P[ 3], S33, $D4EF3085);  { 43 }
  HH(B, C, D, A, P[ 6], S34,  $4881D05);  { 44 }
  HH(A, B, C, D, P[ 9], S31, $D9D4D039);  { 45 }
  HH(D, A, B, C, P[12], S32, $E6DB99E5);  { 46 }
  HH(C, D, A, B, P[15], S33, $1FA27CF8);  { 47 }
  HH(B, C, D, A, P[ 2], S34, $C4AC5665);  { 48 }

  {round 4}
  II(A, B, C, D, P[ 0], S41, $F4292244);  { 49 }
  II(D, A, B, C, P[ 7], S42, $432AFF97);  { 50 }
  II(C, D, A, B, P[14], S43, $AB9423A7);  { 51 }
  II(B, C, D, A, P[ 5], S44, $FC93A039);  { 52 }
  II(A, B, C, D, P[12], S41, $655B59C3);  { 53 }
  II(D, A, B, C, P[ 3], S42, $8F0CCC92);  { 54 }
  II(C, D, A, B, P[10], S43, $FFEFF47D);  { 55 }
  II(B, C, D, A, P[ 1], S44, $85845DD1);  { 56 }
  II(A, B, C, D, P[ 8], S41, $6FA87E4F);  { 57 }
  II(D, A, B, C, P[15], S42, $FE2CE6E0);  { 58 }
  II(C, D, A, B, P[ 6], S43, $A3014314);  { 59 }
  II(B, C, D, A, P[13], S44, $4E0811A1);  { 60 }
  II(A, B, C, D, P[ 4], S41, $F7537E82);  { 61 }
  II(D, A, B, C, P[11], S42, $BD3AF235);  { 62 }
  II(C, D, A, B, P[ 2], S43, $2AD7D2BB);  { 63 }
  II(B, C, D, A, P[ 9], S44, $EB86D391);  { 64 }

  Inc(FRegs[0], A);
  Inc(FRegs[1], B);
  Inc(FRegs[2], C);
  Inc(FRegs[3], D);
end;

procedure TMD5.Update(const Buffer; Length: Integer);
var
  I: Integer;
begin 
  Inc(FTotal, Length);
  for I := 0 to Length - 1 do
  begin
    FBuffer[FLength] := TByteArray(Buffer)[I];
    Inc(FLength);
    if FLength >= SizeOf(FBuffer) then
    begin
      Transform;
      FLength := 0;
    end;
  end;
end;

procedure TMD5.Finalize;
var
  Length: Int64;
begin
  FillChar(FBuffer[FLength], SizeOf(FBuffer) - FLength, 0);
  FBuffer[FLength] := $80;

  if FLength >= 56 then
  begin
    Transform;
    FillChar(FBuffer[0], SizeOf(FBuffer), 0);
  end;

  Length := Int64(FTotal) shl 3;
  Move(Length, FBuffer[56], SizeOf(Length));

  Transform;
end;

function TMD5.GetDigest: TDigest;
begin
  Result := FDigest;
end;

{ === TSHA1 ================================================== }

constructor TSHA1.Create;
begin
  FDigest := TDigest.Create(@FRegs[0], SizeOf(FRegs));
  Reset;
end;

destructor TSHA1.Destroy;
begin
  FDigest.Free;
  inherited;
end;

procedure TSHA1.Reset;
begin
  FLength := 0;
  FTotal := 0;
  FRegs[0] := $67452301;
  FRegs[1] := $EFCDAB89;
  FRegs[2] := $98BADCFE;
  FRegs[3] := $10325476;
  FRegs[4] := $C3D2E1F0;
end;

procedure TSHA1.Transform;

  function F0(B, C, D: Cardinal): Cardinal;
  begin
    Result := (B and C) or (not B and D);
  end;

  function F1(B, C, D: Cardinal): Cardinal;
  begin
    Result := B xor C xor D;
  end;

  function F2(B, C, D: Cardinal): Cardinal;
  begin
    Result := (B and C) or (B and D) or (C and D);
  end;

  function F3(B, C, D: Cardinal): Cardinal;
  begin
    Result := B xor C xor D;
  end;

var
  A, B, C, D, E, X: Cardinal;
  W: array [0..79] of Cardinal;
  I: Integer;
begin
  A := FRegs[0];
  B := FRegs[1];
  C := FRegs[2];
  D := FRegs[3];
  E := FRegs[4];

  Move(FBuffer, W, SizeOf(FBuffer));
  for I := 0 to 15 do
    W[I] := SwapBytes(W[I]);
  for I := 16 to 79 do
    W[I] := Rol(W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16], 1);

  for I := 0 to 19 do
  begin
    X := Rol(A, 5) + F0(B, C, D) + E + W[I] + $5A827999;
    E := D;
    D := C;
    C := Rol(B, 30);
    B := A;
    A := X;
  end;

  for I := 20 to 39 do
  begin
    X := Rol(A, 5) + F1(B, C, D) + E + W[I] + $6ED9EBA1;
    E := D;
    D := C;
    C := Rol(B, 30);
    B := A;
    A := X;
  end;

  for I := 40 to 59 do
  begin
    X := Rol(A, 5) + F2(B, C, D) + E + W[I] + $8F1BBCDC;
    E := D;
    D := C;
    C := Rol(B, 30);
    B := A;
    A := X;
  end;

  for I := 60 to 79 do
  begin
    X := Rol(A, 5) + F3(B, C, D) + E + W[I] + $CA62C1D6;
    E := D;
    D := C;
    C := Rol(B, 30);
    B := A;
    A := X;
  end;

  Inc(FRegs[0], A);
  Inc(FRegs[1], B);
  Inc(FRegs[2], C);
  Inc(FRegs[3], D);
  Inc(FRegs[4], E);
end;

procedure TSHA1.Update(const Buffer; Length: Integer);
var
  I: Integer;
begin 
  Inc(FTotal, Length);
  for I := 0 to Length - 1 do
  begin
    FBuffer[FLength] := TByteArray(Buffer)[I];
    Inc(FLength);
    if FLength >= SizeOf(FBuffer) then
    begin
      Transform;
      FLength := 0;
    end;
  end;
end;

procedure TSHA1.Finalize;
var
  Length: Int64;
begin
  FillChar(FBuffer[FLength], SizeOf(FBuffer) - FLength, 0);
  FBuffer[FLength] := $80;

  if FLength >= 56 then
  begin
    Transform;
    FillChar(FBuffer[0], SizeOf(FBuffer), 0);
  end;

  Length := SwapBytes(Int64(FTotal) shl 3);
  Move(Length, FBuffer[56], SizeOf(Length));

  Transform;

  FRegs[0] := SwapBytes(FRegs[0]);
  FRegs[1] := SwapBytes(FRegs[1]);
  FRegs[2] := SwapBytes(FRegs[2]);
  FRegs[3] := SwapBytes(FRegs[3]);
  FRegs[4] := SwapBytes(FRegs[4]);
end;

function TSHA1.GetDigest: TDigest;
begin
  Result := FDigest;
end;

end.
