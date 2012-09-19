unit uRSA;

interface

uses
  uBigNum, uHash;

const
  RSA_GEN_P = 0;
  RSA_GEN_Q = 1;

type
  THashMethod = ( hmMD5, hmSHA1 );

type
  TRSACallback = TBigNumCallback;

  TRSA = class
  private
    FD, FE, FN: TBigNum;
    FCallback: TBigNumCallback;
    FUserData: Integer;
    FState: Integer;
    FHashMethod: THashMethod;
  public
    constructor Create;
    destructor Destroy; override;

    property D: TBigNum read FD;
    property E: TBigNum read FE;
    property N: TBigNum read FN;
    property HashMethod: THashMethod read FHashMethod write FHashMethod;

    procedure SavePrivateKey(FileName: string);
    procedure LoadPrivateKey(FileName: string);

    procedure SavePublicKey(FileName: string);
    procedure LoadPublicKey(FileName: string);

    function RandomKey(Bits: Integer; E: TDigit; Callback: TRSACallback;
        UserData: Integer): Boolean;
    function GenerateKey(const P, Q: TBigNum; E: TDigit): Boolean;

    function Sign(const Buffer; Length: Integer; var Sig; var SigLen: Integer): Boolean;
    function Verify(const Buffer; Length: Integer; const Sig; SigLen: Integer): Boolean;

    procedure Encrypt(N: TBigNum);
    procedure Decrypt(N: TBigNum);
  end;

implementation

function BigNumCallback(State, Data, UserData: Integer): Boolean;
var
  RSA: TRSA;
begin
  Result := True;  
  RSA := TRSA(UserData);
  if Assigned(RSA.FCallback) then
  begin
    if not RSA.FCallback(RSA.FState, Data, RSA.FUserData) then
      Result := False;
  end;
end;

constructor TRSA.Create;
begin
  FD := TBigNum.Create;
  FE := TBigNum.Create;
  FN := TBigNum.Create;
end;

destructor TRSA.Destroy;
begin
  FD.Free;
  FN.Free;
  FE.Free;
end;

function TRSA.RandomKey(Bits: Integer; E: TDigit; Callback: TBigNumCallback;
    UserData: Integer): Boolean;
var
  p, q: TBigNum;
begin
  Result := False;

  FCallback := Callback;
  FUserData := UserData;

  p := TBigNum.Create;
  q := TBigNum.Create;
  try
    repeat
      repeat
        FState := RSA_GEN_P;
        if not p.RandomPrime(Bits shr 1, 5, BigNumCallback, Integer(Self)) then
          Exit;

        FState := RSA_GEN_Q;
        if not q.RandomPrime(Bits shr 1, 5, BigNumCallback, Integer(Self)) then
          Exit;
      until p.Compare(q) <> 0;
    until GenerateKey(p, q, E);
    Result := True;
  finally
    p.Free;
    q.Free;
  end;
end;

function TRSA.GenerateKey(const P, Q: TBigNum; E: TDigit): Boolean;
var
  A, B: TBigNum;
begin
  A := P.Clone;
  B := Q.Clone;

  FN.Copy(A);
  FN.Mul(B);

  A.Sub(1);
  B.Sub(1);
  A.Mul(B);

  FE.CopyDigit(E);
  FD.CopyDigit(E);
  Result := FD.ModInv(A) and not FD.Sign;

  A.Free;
  B.Free;
end;

function TRSA.Sign(const Buffer; Length: Integer; var Sig; var SigLen: Integer): Boolean;
var
  Hash: THash;
  Digest: TDigest;
  N: TBigNum;
  I: Integer;
begin
  Result := False;

  if FHashMethod = hmMD5 then
    Hash := TMD5.Create
  else { FHashMethod = hmSHA1 }
    Hash := TSHA1.Create;

  if SigLen < Hash.Digest.Length then
    Exit;

  Digest := Hash.Hash(Buffer, Length);
  SigLen := Digest.Length;
  N := TBigNum.CreateBytes(Digest.Buffer, Digest.Length);
  N.Mod_(FN);
  Decrypt(N);

  for I := 0 to N.Count - 1 do
    TDigitArray(Sig)[I] := N.Digit[I];

  Hash.Free;
  N.Free;
end;

function TRSA.Verify(const Buffer; Length: Integer; const Sig; SigLen: Integer): Boolean;
var
  Hash: THash;
  Digest: TDigest;
  N1, N2: TBigNum;
begin
  if FHashMethod = hmMD5 then
    Hash := TMD5.Create
  else { FHashMethod = hmSHA1 }
    Hash := TSHA1.Create;

  Digest := Hash.Hash(Buffer, Length);
  N1 := TBigNum.CreateBytesBE(Digest.Buffer, Digest.Length);
  N1.Mod_(FN);

  N2 := TBigNum.CreateBytesBE(@Sig, SigLen);
  Encrypt(N2);

  N2.BitCount := Digest.Length * 8;
  Result := N1.Compare(N2) = 0;

  N1.Free;
  N2.Free;
  Hash.Free;
end;

procedure TRSA.Encrypt(N: TBigNum);
begin
  N.PowMod(FE, FN);
end;

procedure TRSA.Decrypt(N: TBigNum);
begin
  N.PowMod(FD, FN);
end;

procedure TRSA.SavePrivateKey(FileName: string);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-} Rewrite(F); {$I+}
  if IOResult <> 0 then
    Exit;
  Writeln(F, D.ToHex);
  Writeln(F, E.ToHex);
  Writeln(F, N.ToHex);
  CloseFile(F);
end;

procedure TRSA.LoadPrivateKey(FileName: string);
var
  F: TextFile;
  S: string;
begin
  D.Clear;
  E.Clear;
  N.Clear;

  AssignFile(F, FileName);
  FileMode := 0;
  {$I-} Reset(F); {$I+}
  if IOResult <> 0 then
    Exit;

  Readln(F, S);
  D.CopyHex(S);
  Readln(F, S);
  E.CopyHex(S);
  Readln(F, S);
  N.CopyHex(S);

  CloseFile(F);
end;

procedure TRSA.SavePublicKey(FileName: string);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-} Rewrite(F); {$I+}
  if IOResult <> 0 then
    Exit;
  Writeln(F, E.ToHex);
  Writeln(F, N.ToHex);
  CloseFile(F);
end;

procedure TRSA.LoadPublicKey(FileName: string);
var
  F: TextFile;
  S: string;
begin
  E.Clear;
  N.Clear;

  AssignFile(F, FileName);
  FileMode := 0;
  {$I-} Reset(F); {$I+}
  if IOResult <> 0 then
    Exit;

  Readln(F, S);
  E.CopyHex(S);
  Readln(F, S);
  N.CopyHex(S);

  CloseFile(F);
end;

end.

