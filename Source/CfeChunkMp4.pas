unit CfeChunkMp4;

interface

uses
  Classes, SysUtils, CfeChunkCommon;

type
  TMp4DefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4FixedDefinedChunk = class(TFixedDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4UnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TMp4ContainerChunk = class(TChunkContainer)
  public
    constructor Create; override;
  end;

  TMp4FileTypeChunk = class(TMp4DefinedChunk)
  private
    FMajorBrand: TChunkName;
    FMinorVersion: Integer;
    FCompatibleBrands: array of TChunkName;
    function GetCompatibleBrandsAsString: String;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property MajorBrand: TChunkName read FMajorBrand;
    property MinorVersion: Integer read FMinorVersion;
    property CompatibleBrandsAsString: String read GetCompatibleBrandsAsString;
  end;

  TMp4FreeChunk = class(TMp4DefinedChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MoovChunk = class(TMp4ContainerChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  public
    constructor Create; override;
  end;

  TMp4MovieHeader = record
    Version: Byte;
    Flags: array [0..2] of Byte;
    CreationTime: Cardinal;
    ModificationTime: Cardinal;
    TimeScale: Cardinal;
    Duration: Cardinal;
    PreferredRate: Cardinal;
    PreferredVolume: Word;
    Reserved: array [0..9] of Byte;
    MatrixStructure: array [0..8] of Integer;
    PreviewTime: Cardinal;
    PreviewDuration: Cardinal;
    PosterTime: Cardinal;
    SelectionTime: Cardinal;
    SelectionDuration: Cardinal;
    CurrentTime: Cardinal;
    NextTrackId: Cardinal;
  end;

  TMp4MovieHeaderChunk = class(TMp4FixedDefinedChunk)
  private
    FMovieHeader: TMp4MovieHeader;
    function GetCreationTime: TDateTime;
    function GetDuration: Single;
    function GetModificationTime: TDateTime;
    function GetPreferredRate: Single;
    function GetPreferredVolume: Single;
    function GetTimeScale: Single;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    class function GetClassChunkSize: Cardinal; override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Single read GetTimeScale;
    property Duration: Single read GetDuration;
    property PreferredRate: Single read GetPreferredRate;
    property PreferredVolume: Single read GetPreferredVolume;
  end;

  TMp4TrackChunk = class(TMp4DefinedChunk)
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
  end;

  TFileMp4 = class(TChunkedFile)
  private
    FFileType: TMp4FileTypeChunk;
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
      ChunkSize: Cardinal);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
    class function CanLoad(Stream: TStream): Boolean;
  end;

implementation

uses
  DateUtils;

{ TMp4DefinedChunk }

constructor TMp4DefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4UnknownChunk }

constructor TMp4UnknownChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FixedDefinedChunk }

constructor TMp4FixedDefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4ContainerChunk }

constructor TMp4ContainerChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FreeChunk }

class function TMp4FreeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'free';
end;


{ TMp4FileTypeChunk }

class function TMp4FileTypeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

function TMp4FileTypeChunk.GetCompatibleBrandsAsString: String;
var
  Index: Integer;
begin
  if Length(FCompatibleBrands) = 0 then
    Result := '';

  Result := FCompatibleBrands[0];

  for Index := 1 to High(FCompatibleBrands) do
    Result := Result + ', ' + FCompatibleBrands[Index];
end;

procedure TMp4FileTypeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  CompatibleBrandCount: Integer;
begin
  inherited;

  Stream.Read(FMajorBrand, 4);
  Stream.Read(FMinorVersion, 4);
  CompatibleBrandCount := (FChunkSize - 16) div SizeOf(Integer);
  SetLength(FCompatibleBrands, CompatibleBrandCount);
  for Index := 0 to CompatibleBrandCount - 1 do
    Stream.Read(FCompatibleBrands[Index], 4);
end;

procedure TMp4FileTypeChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FMajorBrand, 4);
  Stream.Write(FMinorVersion, 4);
  for Index := 0 to Length(FCompatibleBrands) - 1 do
    Stream.Write(FCompatibleBrands[Index], 4);
end;


{ TMp4MoovChunk }

constructor TMp4MoovChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MovieHeaderChunk, TMp4TrackChunk]);
end;

class function TMp4MoovChunk.GetClassChunkName: TChunkName;
begin
  Result := 'moov';
end;


{ TMp4MovieHeaderChunk }

constructor TMp4MovieHeaderChunk.Create;
begin
  inherited;

  // set initial values
  with FMovieHeader do
  begin
    Version := 0;
    Flags[0] := 0;
    Flags[1] := 0;
    Flags[2] := 0;
    CreationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
    ModificationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
    TimeScale := $3E8;
    Duration := $57BC0;
    PreferredRate := $10000;
    PreferredVolume := $100;
    FillChar(Reserved[0], 10, 0);
    MatrixStructure[0] := $100;
    MatrixStructure[1] := 0;
    MatrixStructure[2] := 0;
    MatrixStructure[3] := 0;
    MatrixStructure[4] := $100;
    MatrixStructure[5] := 0;
    MatrixStructure[6] := 0;
    MatrixStructure[7] := 0;
    MatrixStructure[8] := $40;
    PreviewTime := 0;
    PreviewDuration := 0;
    PosterTime := 0;
    SelectionTime := 0;
    SelectionDuration := 0;
    CurrentTime := 0;
    NextTrackId := $2000000;
  end;

  StartAddress := @FMovieHeader;
end;

class function TMp4MovieHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mvhd';
end;

class function TMp4MovieHeaderChunk.GetClassChunkSize: Cardinal;
begin
  Result := SizeOf(TMp4MovieHeader);
end;

function TMp4MovieHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FMovieHeader.CreationTime);
end;

function TMp4MovieHeaderChunk.GetDuration: Single;
begin
  Result :=  FMovieHeader.Duration / FMovieHeader.TimeScale;
end;

function TMp4MovieHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FMovieHeader.ModificationTime);
end;

function TMp4MovieHeaderChunk.GetPreferredRate: Single;
begin
  Result :=  FMovieHeader.PreferredRate / $10000;
end;

function TMp4MovieHeaderChunk.GetPreferredVolume: Single;
begin
  Result :=  FMovieHeader.PreferredVolume / $100;
end;

function TMp4MovieHeaderChunk.GetTimeScale: Single;
begin
  Result := 1  / FMovieHeader.TimeScale;
end;


{ TMp4TrackChunk }

class function TMp4TrackChunk.GetClassChunkName: TChunkName;
begin
  Result := 'trak';
end;

procedure TMp4TrackChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Position := Stream.Position + ChunkSize;
end;


{ TFileMp4 }

constructor TFileMp4.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder];
  FFileType := TMp4FileTypeChunk.Create;
end;

destructor TFileMp4.Destroy;
begin

  inherited;
end;

class function TFileMp4.CanLoad(Stream: TStream): Boolean;
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
begin
  Result := Stream.Size >= 8;

  if Result then
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Stream.Seek(-8, soFromCurrent);
    Result := ChunkName = GetClassChunkName;
  end;
end;

class function TFileMp4.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

procedure TFileMp4.ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
  ChunkSize: Cardinal);
var
  ChunkClass: TDefinedChunkClass;
  DefinedChunk: TDefinedChunk;
  UnknownChunk: TMp4UnknownChunk;
begin
  ChunkClass := ChunkClassByChunkName(ChunkName);
  if Assigned(ChunkClass) then
  begin
    DefinedChunk := ChunkClass.Create;
    DefinedChunk.LoadFromStream(Stream);
    FChunkList.Add(DefinedChunk);
  end
  else
  begin
    UnknownChunk := TMp4UnknownChunk.Create;
    UnknownChunk.LoadFromStream(Stream);
    AddChunk(UnknownChunk);
  end;
end;

procedure TFileMp4.LoadFromStream(Stream: TStream);
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
  UnknownChunk: TMp4UnknownChunk;
begin
  FFileType.LoadFromStream(Stream);
  AddChunk(FFileType);

  while Stream.Position + 8 <= Stream.Size do
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Flip32(ChunkSize);
    Stream.Position := Stream.Position - 8;
    ReadUnknownChunk(Stream, ChunkName, ChunkSize);
  end;
end;

procedure TFileMp4.SaveToStream(Stream: TStream);
begin
  FFileType.SaveToStream(Stream);
end;

initialization

  TFileMp4.RegisterChunks([TMp4FileTypeChunk, TMp4FreeChunk, TMp4MoovChunk,
    TMp4MovieHeaderChunk, TMp4TrackChunk]);

end.
