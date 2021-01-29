unit CfeChunkPng;

interface

uses
  Classes, Graphics, SysUtils, CfeChunkCommon,
  {$IFDEF FPC} ZBase, ZDeflate, ZInflate; {$ELSE}
  {$IFDEF ZLibEx}ZLibEx, ZLibExApi; {$ELSE}
  {$IFDEF COMPILERRX2_UP} System.zlib; {$ELSE} zlib;
  {$ENDIF}{$ENDIF}{$ENDIF}

type
  {$A1}
  TColorType = (
    ctGrayscale = 0,
    ctTrueColor = 2,
    ctIndexedColor = 3,
    ctGrayscaleAlpha = 4,
    ctTrueColorAlpha = 6
  );

  TFilterMethod = (
    fmAdaptiveFilter = 0
  );

  TAdaptiveFilterMethod = (
    afmNone = 0,
    afmSub = 1,
    afmUp = 2,
    afmAverage = 3,
    afmPaeth = 4
  );

  TAvailableAdaptiveFilterMethod = (aafmSub, aafmUp, aafmAverage, aafmPaeth);
  TAvailableAdaptiveFilterMethods = set of TAvailableAdaptiveFilterMethod;

  TInterlaceMethod = (
    imNone = 0,
    imAdam7 = 1
  );

  TRGB24 = packed record
    R, G, B: Byte;
  end;
  PRGB24 = ^TRGB24;
  TRGB24Array = array [0..0] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TRGB24Word = packed record
    R, G, B : Word;
  end;
  PRGB24Word = ^TRGB24Word;

  TRGB32 = packed record
    R, G, B, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRGB32Word = packed record
    R, G, B, A: Word;
  end;
  PRGB32Word = ^TRGB32Word;

  PByteArray = SysUtils.PByteArray;
  TByteArray = SysUtils.TByteArray;

  EPngError = class(Exception);

  {$IFDEF FPC}
  TZStreamRec = z_stream;
  {$ENDIF}

  {$A4}

  TChunkPngImageHeader = class(TDefinedChunk)
  private
    FWidth                 : Integer;
    FHeight                : Integer;
    FBitDepth              : Byte;
    FColorType             : TColorType;
    FCompressionMethod     : Byte;
    FFilterMethod          : TFilterMethod;
    FInterlaceMethod       : TInterlaceMethod;
    FAdaptiveFilterMethods : TAvailableAdaptiveFilterMethods;
    function GetHasPalette: Boolean;
    function GetBytesPerRow: Integer;
    function GetPixelByteSize: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    procedure ReadFromStream(Stream: TStream); virtual;
    procedure WriteToStream(Stream: TStream); virtual;

    class function GetClassChunkName: TChunkName; override;

    procedure ResetToDefault; virtual;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColorType: TColorType read FColorType write FColorType;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read FAdaptiveFilterMethods;
    property FilterMethod: TFilterMethod read FFilterMethod;
    property InterlaceMethod: TInterlaceMethod read FInterlaceMethod;
    property HasPalette: Boolean read GetHasPalette;

    property BytesPerRow: Integer read GetBytesPerRow;
    property PixelByteSize: Integer read GetPixelByteSize;
  end;

  TCustomDefinedChunkWithHeader = class(TDefinedChunk)
  protected
    FHeader : TChunkPngImageHeader;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); reintroduce; virtual;

    procedure HeaderChanged; virtual;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property Header: TChunkPngImageHeader read FHeader;
  end;
  TCustomDefinedChunkWithHeaderClass = class of TCustomDefinedChunkWithHeader;

  TChunkPngImageData = class(TCustomDefinedChunkWithHeader)
  private
    FData : TMemoryStream;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property Data: TMemoryStream read FData;
  end;

  TChunkPngPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteEntries : array of TRGB24;
    function GetPaletteEntry(Index: Cardinal): TRGB24;
    function GetCount: Cardinal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
    procedure PaletteEntriesChanged; virtual;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property PaletteEntry[Index: Cardinal]: TRGB24 read GetPaletteEntry; default;
    property Count: Cardinal read GetCount;
  end;

  TChunkPngGamma = class(TCustomDefinedChunkWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle;
  end;

  TChunkPngStandardColorSpaceRGB = class(TCustomDefinedChunkWithHeader)
  private
    FRenderingIntent : Byte;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property RenderingIntent: Byte read FRenderingIntent write FRenderingIntent;
  end;

  TChunkPngPrimaryChromaticities = class(TCustomDefinedChunkWithHeader)
  private
    FWhiteX : Integer;
    FWhiteY : Integer;
    FRedX   : Integer;
    FRedY   : Integer;
    FGreenX : Integer;
    FGreenY : Integer;
    FBlueX  : Integer;
    FBlueY  : Integer;
    function GetBlueX: Single;
    function GetBlueY: Single;
    function GetGreenX: Single;
    function GetGreenY: Single;
    function GetRedX: Single;
    function GetRedY: Single;
    function GetWhiteX: Single;
    function GetWhiteY: Single;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property WhiteX: Integer read FWhiteX write FWhiteX;
    property WhiteY: Integer read FWhiteY write FWhiteY;
    property RedX: Integer read FRedX write FRedX;
    property RedY: Integer read FRedY write FRedY;
    property GreenX: Integer read FGreenX write FGreenX;
    property GreenY: Integer read FGreenY write FGreenY;
    property BlueX: Integer read FBlueX write FBlueX;
    property BlueY: Integer read FBlueY write FBlueY;

    property WhiteXAsSingle: Single read GetWhiteX;
    property WhiteYAsSingle: Single read GetWhiteY;
    property RedXAsSingle: Single read GetRedX;
    property RedYAsSingle: Single read GetRedY;
    property GreenXAsSingle: Single read GetGreenX;
    property GreenYAsSingle: Single read GetGreenY;
    property BlueXAsSingle: Single read GetBlueX;
    property BlueYAsSingle: Single read GetBlueY;
  end;

  TChunkPngTime = class(TCustomDefinedChunkWithHeader)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
    function GetModifiedDateTime: TDateTime;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
    property ModifiedDateTime: TDateTime read GetModifiedDateTime;
  end;

  TChunkPngEmbeddedIccProfile = class(TCustomDefinedChunkWithHeader)
  private
    FProfileName       : AnsiString;
    FCompressionMethod : Byte;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TCustomPngSignificantBits = class(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    constructor Create(BitDepth: Integer = 8); virtual; abstract;

    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits  : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TPngSignificantBitsFormat6 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TChunkPngSignificantBits = class(TCustomDefinedChunkWithHeader)
  private
    FSignificantBits : TCustomPngSignificantBits;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    procedure HeaderChanged; override;

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

  TCustomPngBackgroundColor = class(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  private
    FIndex : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomDefinedChunkWithHeader)
  private
    FBackground : TCustomPngBackgroundColor;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

  TChunkPngImageHistogram = class(TCustomDefinedChunkWithHeader)
  private
    FHistogram : array of Word;
    function GetCount: Cardinal;
    function GetFrequency(Index: Cardinal): Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property Count: Cardinal read GetCount;
    property Frequency[Index: Cardinal]: Word read GetFrequency;
  end;

  TSuggestedPalette8ByteEntry = record
    Red       : Byte;
    Green     : Byte;
    Blue      : Byte;
    Alpha     : Byte;
    Frequency : Word;
  end;
  PSuggestedPalette8ByteEntry = ^TSuggestedPalette8ByteEntry;
  TSuggestedPalette8ByteArray = array [0..0] of TSuggestedPalette8ByteEntry;
  PSuggestedPalette8ByteArray = ^TSuggestedPalette8ByteArray;

  TSuggestedPalette16ByteEntry = record
    Red       : Word;
    Green     : Word;
    Blue      : Word;
    Alpha     : Word;
    Frequency : Word;
  end;
  PSuggestedPalette16ByteEntry = ^TSuggestedPalette16ByteEntry;
  TSuggestedPalette16ByteArray = array [0..0] of TSuggestedPalette16ByteEntry;
  PSuggestedPalette16ByteArray = ^TSuggestedPalette16ByteArray;

  TChunkPngSuggestedPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteName : AnsiString;
    FData        : Pointer;
    FCount       : Cardinal;
    FSampleDepth : Byte;
    function GetCount: Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property PaletteName: AnsiString read FPaletteName;
    property Count: Cardinal read GetCount;
  end;

  TCustomPngTransparency = class(TPersistent)
  protected
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Cardinal;
    function GetTransparency(Index: Cardinal): Byte;
  protected
    FTransparency : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
    property Transparency[Index: Cardinal]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomDefinedChunkWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomDefinedChunkWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TChunkPngPhysicalScale = class(TCustomDefinedChunkWithHeader)
  private
    FUnitSpecifier  : Byte;
    FUnitsPerPixelX : Single;
    FUnitsPerPixelY : Single;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property UnitsPerPixelX: Single read FUnitsPerPixelX write FUnitsPerPixelX;
    property UnitsPerPixelY: Single read FUnitsPerPixelY write FUnitsPerPixelY;
  end;

  TChunkPngImageOffset = class(TCustomDefinedChunkWithHeader)
  private
    FImagePositionX : Integer;
    FImagePositionY : Integer;
    FUnitSpecifier  : Byte;
  protected
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property ImagePositionX: Integer read FImagePositionX write FImagePositionX;
    property ImagePositionY: Integer read FImagePositionY write FImagePositionY;
  end;

  TChunkPngPixelCalibrator = class(TCustomDefinedChunkWithHeader)
  private
    FCalibratorName : AnsiString;
    FOriginalZeroes : array [0..1] of Integer;
    FEquationType   : Byte;
    FNumberOfParams : Byte;
    FUnitName       : AnsiString;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property CalibratorName: AnsiString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: Integer read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: Integer read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
  end;

  TCustomChunkPngText = class(TCustomDefinedChunkWithHeader)
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
    procedure KeywordChanged; virtual;
    procedure TextChanged; virtual;
  public
    property Keyword: AnsiString read FKeyword;
    property Text: AnsiString read FText;
  end;

  TChunkPngText = class(TCustomChunkPngText)
  protected
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TChunkPngCompressedText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngInternationalText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  protected
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;

  TChunkPngUnknown = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
  protected
    FChunkName  : TChunkName;
    FDataStream : TMemoryStream;
    function GetChunkNameAsString: AnsiString; override;
    function GetChunkSize: Cardinal; override;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ChunkName: TChunkName); reintroduce;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Data[index : Integer]: Byte read GetData;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TChunkList = class(TPersistent)
  private
    FChunks : array of TCustomChunk;
    function GetCount: Cardinal;
  protected
    function GetChunk(Index: Integer): TCustomChunk;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Cardinal);
    function IndexOf(Item: TCustomChunk): Integer;
    procedure Remove(Item: TCustomChunk);

    property Count: Cardinal read GetCount;
    property Chunks[Index: Integer]: TCustomChunk read GetChunk; default;
  end;

  TCustomPngCoder = class
  protected
    FStream       : TStream;
    FHeader       : TChunkPngImageHeader;
    FGamma        : TChunkPngGamma;
    FPalette      : TChunkPngPalette;
    FTransparency : TCustomPngTransparency;

    FRowBuffer    : array [0..1] of PByteArray;
    FAlphaTable   : PByteArray;
    FMappingTable : PByteArray;
    procedure BuildMappingTables; virtual;

    procedure EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure DecodeFilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency : TCustomPngTransparency = nil); virtual;
    destructor Destroy; override;
  end;

  TFilePng = class(TChunkedFile)
  private
    FCompressionLevel : Byte;
    function GetBitDepth: Byte;
    function GetColorType: TColorType;
    function GetCompressionMethod: Byte;
    function GetFilterMethod: TFilterMethod;
    function GetHeight: Integer;
    function GetInterlaceMethod: TInterlaceMethod;
    function GetPaletteEntry(Index: Integer): TRGB24;
    function GetPaletteEntryCount: Integer;
    function GetWidth: Integer;
    function GetGamma: Single;
    function GetModifiedTime: TDateTime;
    function GetPixelsPerUnitX: Cardinal;
    function GetPixelsPerUnitY: Cardinal;
    function GetPixelUnit: Byte;

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TStream): Cardinal; overload;
    {$IFDEF CheckCRC}
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    {$ENDIF}
    procedure ReadImageDataChunk(Stream: TStream; Size: Integer);
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName; ChunkSize: Integer);
    function GetFilterMethods: TAvailableAdaptiveFilterMethods;
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FGammaChunk          : TChunkPngGamma;
    FTimeChunk           : TChunkPngTime;
    FSignificantBits     : TChunkPngSignificantBits;
    FPhysicalDimensions  : TChunkPngPhysicalPixelDimensions;
    FChromaChunk         : TChunkPngPrimaryChromaticities;
    FTransparencyChunk   : TChunkPngTransparency;
    FBackgroundChunk     : TChunkPngBackgroundColor;
    FDataChunkList       : TChunkList;
    FAdditionalChunkList : TChunkList;

    procedure Clear; virtual;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CopyImageData(Stream: TStream);
    procedure StoreImageData(Stream: TStream);
    procedure DecompressImageDataToStream(Stream: TStream);
    procedure CompressImageDataFromStream(Stream: TStream);

    property ImageHeader: TChunkPngImageHeader read FImageHeader;
    property PaletteChunk: TChunkPngPalette read FPaletteChunk;
    property TransparencyChunk: TChunkPngTransparency read FTransparencyChunk;
    property BackgroundChunk: TChunkPngBackgroundColor read FBackgroundChunk;
    property GammaChunk: TChunkPngGamma read FGammaChunk;
    property TimeChunk: TChunkPngTime read FTimeChunk;
    property PhysicalPixelDimensionsChunk: TChunkPngPhysicalPixelDimensions read FPhysicalDimensions;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function CanLoad(Stream: TStream): Boolean;
    class function GetClassChunkName: TChunkName; override;

    function HasPhysicalPixelDimensionsInformation: Boolean;
    function HasGammaInformation: Boolean;
    function HasModifiedTimeInformation: Boolean;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property BitDepth: Byte read GetBitDepth;
    property ColorType: TColorType read GetColorType;
    property CompressionMethod: Byte read GetCompressionMethod;
    property CompressionLevel: Byte read FCompressionLevel;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read GetFilterMethods;
    property FilterMethod: TFilterMethod read GetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod;
    property PaletteEntry[Index: Integer]: TRGB24 read GetPaletteEntry;
    property PaletteEntryCount: Integer read GetPaletteEntryCount;
    property Gamma: Single read GetGamma;
    property ModifiedTime: TDateTime read GetModifiedTime;
    property PixelsPerUnitX: Cardinal read GetPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read GetPixelsPerUnitY;
    property PixelUnit: Byte read GetPixelUnit;

    property SignificantBitsChunk: TChunkPngSignificantBits read FSignificantBits;
    property PrimaryChromaticitiesChunk: TChunkPngPrimaryChromaticities read FChromaChunk;
  end;

function ColorTypeToString(Value: TColorType): string;
function InterlaceMethodToString(Value: TInterlaceMethod): string;

implementation

uses
  Math;

resourcestring
  RCStrAncillaryUnknownChunk = 'Unknown chunk is marked as ancillary';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  RCStrDataIncomplete = 'Data not complete';
  RCStrDirectCompressionMethodSetError = 'Compression Method may not be specified directly yet!';
  RCStrDirectFilterMethodSetError = 'Filter Method may not be specified directly yet!';
  RCStrDirectGammaSetError = 'Gamma may not be specified directly yet!';
  RCStrDirectHeightSetError = 'Height may not be specified directly yet!';
  RCStrDirectWidthSetError = 'Width may not be specified directly yet!';
  RCStrEmptyChunkList = 'Chunk list is empty';
  RCStrHeaderInvalid = 'The provided header is not valid!';
  RCStrIncompletePalette = 'Palette is incomplete';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrNewHeaderError = 'New header may not be nil!';
  RCStrNotAValidPNGFile = 'Not a valid PNG file';
  RCStrNotYetImplemented = 'Not yet implemented';
  RCStrPaletteLimited = 'Palette is limited to 256 entries';
  RCStrSeveralChromaChunks = 'Primary chromaticities chunk defined twice!';
  RCStrSeveralGammaChunks = 'Gamma chunk defined twice!';
  RCStrSeveralPaletteChunks = 'Palette chunk defined twice!';
  RCStrSeveralTransparencyChunks = 'Transparency chunk defined twice!';
  RCStrSeveralBackgroundChunks = 'Background chunk defined twice!';
  RCStrSeveralPhysicalPixelDimensionChunks = 'Several physical pixel dimenson chunks found';
  RCStrSeveralSignificantBitsChunksFound = 'Several significant bits chunks found';
  RCStrSeveralTimeChunks = 'Time chunk appears twice!';
  RCStrUnknownColorType = 'Unknown color type!';
  RCStrUnspecifiedPixelUnit = 'Unspecified unit';
  RCStrUnsupportedCompressionMethod = 'Compression method not supported!';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrWrongInterlaceMethod = 'Wrong interlace method';
  RCStrWrongPixelPerUnit = 'Pixel per unit may not be zero!';
  RCStrWrongTransparencyFormat = 'Wrong transparency format';
  RCStrInvalidCompressionLevel = 'Invalid compression level';
  RCStrBitDepthTranscodingError = 'Bit depth may not be specified directly yet!';
  RCStrColorTypeTranscodingError = 'Color Type may not be specified directly yet!';
  RCStrGrayscale = 'Grayscale';
  RCStrTrueColor = 'True Color';
  RCStrIndexedColor = 'Indexed Color';
  RCStrGrayscaleAlpha = 'Transparent Grayscale';
  RCStrTrueColorAlpha = 'Transparent True Color';
  RCStrInterlacingNone = 'None';
  RCStrInterlacingAdam7 = 'Adam7';
  {$IFDEF CheckCRC}
  RCStrCRCError = 'CRC Error';
  {$ENDIF}

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;

const
  CPngMagic = #$0D#$0A#$1A#$0A;


{ Byte Ordering }

type
  T16Bit = record
    case Integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of Byte);
  end;

  T32Bit = record
    case Integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of Byte);
  end;

function Swap16(Value: SmallInt): SmallInt;
var
  t: Byte;
begin
  with T16Bit(Value) do
  begin
    t := b[0];
    b[0] := b[1];
    b[1] := t;
    Result := v;
  end;
end;

function Swap32(Value: LongInt): LongInt;
var
  Temp: Byte;
begin
  with T32Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[3];
    b[3] := Temp;
    Temp := b[1];
    b[1] := b[2];
    b[2] := Temp;
    Result := v;
  end;
end;

procedure Flip16(var Value);
var
  t: Byte;
begin
  with T16Bit(Value) do
  begin
    t := b[0];
    b[0] := b[1];
    b[1] := t;
  end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
  with T32Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[3];
    b[3] := Temp;
    Temp := b[1];
    b[1] := b[2];
    b[2] := Temp;
  end;
end;


{ Stream I/O functions }

function ReadSwappedWord(Stream: TStream): Word;
begin
  {$IFDEF FPC}
  Result := 0;
  {$ENDIF}
  {$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
  {$ELSE}
  Stream.Read(Result, SizeOf(Word));
  {$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TStream): SmallInt;
begin
  {$IFDEF FPC}
  Result := 0;
  {$ENDIF}
  {$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
  {$ELSE}
  Stream.Read(Result, SizeOf(SmallInt));
  {$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TStream): Cardinal;
begin
  {$IFDEF FPC}
  Result := 0;
  {$ENDIF}
  {$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal) then
    raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
  {$ELSE}
  Stream.Read(Result, SizeOf(Cardinal));
  {$ENDIF}
  Result := Swap32(Result);
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

{ conversion }

function ColorTypeToString(Value: TColorType): string;
const
  CColorTypeNames : array [TColorType] of string = (RCStrGrayScale,
    'undefined', RCStrTrueColor, RCStrIndexedColor, RCStrGrayscaleAlpha,
    'undefined', RCStrTrueColorAlpha);
begin
  Result := CColorTypeNames[Value];
end;

function InterlaceMethodToString(Value: TInterlaceMethod): string;
const
  CInterlaceMethodNames : array [TInterlaceMethod] of string = (RCStrInterlacingNone,
    RCStrInterlacingAdam7);
begin
  Result := CInterlaceMethodNames[Value];
end;


{ zlib functions }

procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
  begin
    next_in := Data;
    avail_in := Size;
    {$IFNDEF FPC}
    {$IFNDEF ZLibEx}
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
    {$ENDIF}
    {$ENDIF}
  end;

  {$IFDEF FPC}
  if DeflateInit_(@ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ELSE}
  if DeflateInit_(ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ENDIF}

  GetMem(TempBuffer, CBufferSize);
  try
    while ZStreamRecord.avail_in > 0 do
    begin
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      deflate(ZStreamRecord, Z_NO_FLUSH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := deflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
    FreeMem(TempBuffer);
  end;

  if deflateEnd(ZStreamRecord) > 0 then
    raise EPngError.Create('Error on stream validation');
end;

procedure ZCompress(const Input: TMemoryStream; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
begin
  ZCompress(Input.Memory, Input.Size, Output, Level);
end;

procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
  begin
    next_in := Data;
    avail_in := Size;
    {$IFNDEF FPC}
    {$IFNDEF ZLibEx}
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
    {$ENDIF}
    {$ENDIF}
  end;

  {$IFDEF FPC}
  if inflateInit_(@ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ELSE}
  if inflateInit_(ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ENDIF}

  GetMem(TempBuffer, CBufferSize);
  try
    ZResult := Z_OK;

    while (ZStreamRecord.avail_in > 0) and (ZResult = Z_OK) do
    begin
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := inflate(ZStreamRecord, Z_NO_FLUSH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := inflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
    FreeMem(TempBuffer);
  end;

  if inflateEnd(ZStreamRecord) > 0 then
    raise EPngError.Create('Error on stream validation');
end;

procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;
begin
  ZDecompress(Input.Memory, Input.Size, Output);
end;


{ TChunkPngUnknown }

constructor TChunkPngUnknown.Create(ChunkName: TChunkName);
begin
  FChunkName := ChunkName;
  FDataStream := TMemoryStream.Create;
end;

destructor TChunkPngUnknown.Destroy;
begin
 FreeAndNil(FDataStream);
 inherited;
end;

function TChunkPngUnknown.CalculateChecksum: Integer;
var
  b : Byte;
begin
  with FDataStream do
  begin
    Position := 0;
    Result := 0;
    while Position < Size do
    begin
      Read(b, 1);
      Result := Result + b;
    end;
  end;
end;

procedure TChunkPngUnknown.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TChunkPngUnknown then
  begin
    TChunkPngUnknown(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
    TChunkPngUnknown(Dest).FChunkName := FChunkName;
  end;
end;

function TChunkPngUnknown.GetData(Index: Integer): Byte;
begin
  if (Index >= 0) and (Index < FDataStream.Size) then
    with FDataStream do
    begin
      Position := Index;
      Read(Result, 1);
    end
  else
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [index]);
end;

function TChunkPngUnknown.GetChunkSize: Cardinal;
begin
  Result := FDataStream.Size;
end;

function TChunkPngUnknown.GetChunkNameAsString: AnsiString;
begin
  Result := FChunkName;
end;

procedure TChunkPngUnknown.LoadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    Assert(ChunkSize <= Size);

    FDataStream.Clear;
    FDataStream.Position := 0;
    if ChunkSize > 0 then
      FDataStream.CopyFrom(Stream, ChunkSize);
  end;
end;

procedure TChunkPngUnknown.SaveToStream(Stream: TStream);
begin
  with Stream do
  begin
    FDataStream.Position := 0;
    CopyFrom(FDataStream, FDataStream.Position);
  end;
end;


{ TChunkPngImageHeader }

constructor TChunkPngImageHeader.Create;
begin
  inherited;
  FAdaptiveFilterMethods := [aafmSub, aafmUp, aafmAverage, aafmPaeth];
  FChunkSize := 13;
  ChunkFlags := [cfSizeFirst];

  ResetToDefault;
end;

procedure TChunkPngImageHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageHeader then
  with TChunkPngImageHeader(Dest) do
  begin
    FWidth                 := Self.FWidth;
    FHeight                := Self.FHeight;
    FBitDepth              := Self.FBitDepth;
    FColorType             := Self.FColorType;
    FCompressionMethod     := Self.FCompressionMethod;
    FFilterMethod          := Self.FFilterMethod;
    FInterlaceMethod       := Self.FInterlaceMethod;
    FAdaptiveFilterMethods := Self.FAdaptiveFilterMethods;
  end
  else
    inherited;
end;

function TChunkPngImageHeader.GetBytesPerRow: Integer;
begin
  case FColorType of
    ctGrayscale,
    ctIndexedColor:
      Result := ((FWidth * FBitDepth + $7) and not $7) shr 3;
    ctGrayscaleAlpha:
      Result := 2 * (FBitDepth shr 3) * FWidth;
    ctTrueColor:
      Result := 3 * (FBitDepth shr 3) * FWidth;
    ctTrueColorAlpha:
      Result := 4 * (FBitDepth shr 3) * FWidth;
    else
      raise EPngError.Create(RCStrUnknownColorType);
  end;
end;

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
  Result := 'IHDR';
end;

procedure TChunkPngImageHeader.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read width
    FWidth := ReadSwappedCardinal(Stream);

    // read height
    FHeight := ReadSwappedCardinal(Stream);

    // read bit depth
    Read(FBitDepth, 1);

    // read Color type
    Read(FColorType, 1);

    // check consistency between Color type and bit depth
    case FColorType of
      ctGrayscale:
        if not (FBitDepth in [1, 2, 4, 8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
      ctTrueColor,
      ctGrayscaleAlpha,
      ctTrueColorAlpha:
        if not (FBitDepth in [8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
      ctIndexedColor:
        if not (FBitDepth in [1, 2, 4, 8]) then raise EPngError.Create(RCStrWrongBitdepth);
    end;

    // read compression method
    Read(FCompressionMethod, 1);

    // check for compression method
    if FCompressionMethod <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressMethod);

    // read filter method
    Read(FFilterMethod, 1);

    // check for filter method
    if FFilterMethod <> fmAdaptiveFilter then
      raise EPngError.Create(RCStrUnsupportedFilterMethod);

    // read interlace method
    Read(FInterlaceMethod, 1);

    // check for interlace method
    if not (FInterlaceMethod in [imNone, imAdam7]) then
      raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
  end;
end;

procedure TChunkPngImageHeader.WriteToStream(Stream: TStream);
begin
  with Stream do
  begin
    // write width
    WriteSwappedCardinal(Stream, FWidth);

    // write height
    WriteSwappedCardinal(Stream, FHeight);

    // write bit depth
    Write(FBitDepth, 1);

    // write Color type
    Write(FColorType, 1);

    // write compression method
    Write(FCompressionMethod, 1);

    // write filter method
    Write(FFilterMethod, 1);

    // write interlace method
    Write(FInterlaceMethod, 1);
  end;
end;

function TChunkPngImageHeader.GetPixelByteSize: Integer;
begin
  case ColorType of
    ctGrayscale:
      if FBitDepth = 16 then
        Result := 2
      else
        Result := 1;
    ctTrueColor:
      Result := 3 * FBitDepth div 8;
    ctIndexedColor:
      Result := 1;
    ctGrayscaleAlpha:
      Result := 2 * FBitDepth div 8;
    ctTrueColorAlpha:
      Result := 4 * FBitDepth div 8;
    else
      Result := 0;
  end;
end;

procedure TChunkPngImageHeader.LoadFromStream(Stream: TStream);
begin
  inherited;

  ReadFromStream(Stream);
end;

function TChunkPngImageHeader.GetHasPalette: Boolean;
begin
  Result := FColorType in [ctIndexedColor];
end;

procedure TChunkPngImageHeader.ResetToDefault;
begin
  FWidth             := 0;
  FHeight            := 0;
  FBitDepth          := 8;
  FColorType         := ctTrueColor;
  FCompressionMethod := 0;
  FFilterMethod      := fmAdaptiveFilter;
  FInterlaceMethod   := imNone;
end;

procedure TChunkPngImageHeader.SaveToStream(Stream: TStream);
begin
  inherited;

  WriteToStream(Stream);
end;


{ TCustomDefinedChunkWithHeader }

procedure TCustomDefinedChunkWithHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDefinedChunkWithHeader then
    with TCustomDefinedChunkWithHeader(Dest) do
    begin
      FHeader.Assign(Self.FHeader);
    end
  else
    inherited;
end;

constructor TCustomDefinedChunkWithHeader.Create(Header: TChunkPngImageHeader);
begin
  if not (Header is TChunkPngImageHeader) then
    raise EPngError.Create(RCStrHeaderInvalid);

  ChunkFlags := [cfSizeFirst];

  FHeader := Header;
  inherited Create;
end;

procedure TCustomDefinedChunkWithHeader.HeaderChanged;
begin
 // purely virtual, do nothing by default
end;

procedure TCustomDefinedChunkWithHeader.LoadFromStream(Stream: TStream);
begin
  inherited;

  ReadFromStream(Stream);
end;

procedure TCustomDefinedChunkWithHeader.SaveToStream(Stream: TStream);
begin
  inherited;

  WriteToStream(Stream);
end;


{ TChunkPngPalette }

procedure TChunkPngPalette.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPalette then
    with TChunkPngPalette(Dest) do
    begin
      SetLength(FPaletteEntries, Length(Self.FPaletteEntries));
      Move(Self.FPaletteEntries[0], FPaletteEntries[0], Length(Self.FPaletteEntries) * SizeOf(TRGB24));
    end
  else
    inherited;
end;

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
  Result := 'PLTE';
end;

function TChunkPngPalette.GetPaletteEntry(Index: Cardinal): TRGB24;
begin
  if (Index < Count) then
    Result := FPaletteEntries[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TChunkPngPalette.GetCount: Cardinal;
begin
  Result := Length(FPaletteEntries);
end;

function TChunkPngPalette.GetChunkSize: Cardinal;
begin
  Result := Length(FPaletteEntries) * SizeOf(TRGB24);
end;

procedure TChunkPngPalette.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize mod SizeOf(TRGB24)) <> 0 then
      raise EPngError.Create(RCStrIncompletePalette);

    SetLength(FPaletteEntries, ChunkSize div SizeOf(TRGB24));

    Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
  end;
end;

procedure TChunkPngPalette.WriteToStream(Stream: TStream);
begin
  Stream.Write(FPaletteEntries[0], ChunkSize);
end;

procedure TChunkPngPalette.PaletteEntriesChanged;
begin
  // nothing todo here yet
end;


{ TChunkPngTransparency }

procedure TChunkPngTransparency.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngTransparency then
    with TChunkPngTransparency(Dest) do
    begin
      FTransparency.Assign(Self.FTransparency);
    end
  else
    inherited;
end;

constructor TChunkPngTransparency.Create(Header: TChunkPngImageHeader);
begin
  inherited;
  case Header.ColorType of
    ctGrayscale:
      FTransparency := TPngTransparencyFormat0.Create;
    ctTrueColor:
      FTransparency := TPngTransparencyFormat2.Create;
    ctIndexedColor:
      FTransparency := TPngTransparencyFormat3.Create;
  end;
end;

destructor TChunkPngTransparency.Destroy;
begin
  if Assigned(FTransparency) then
    FreeAndNil(FTransparency);
  inherited;
end;

class function TChunkPngTransparency.GetClassChunkName: TChunkName;
begin
  Result := 'tRNS';
end;

procedure TChunkPngTransparency.HeaderChanged;
var
  OldTransparency : TCustomPngTransparency;
begin
  inherited;

  // store old transparency object
  OldTransparency := FTransparency;

  // change transparency object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
      begin
        FTransparency := TPngTransparencyFormat0.Create;
        if Assigned(OldTransparency) then
        begin
          FTransparency.Assign(OldTransparency);
          FreeAndNil(OldTransparency);
        end;
      end;
  ctTrueColor:
    if not (FTransparency is TPngTransparencyFormat2) then
    begin
      FTransparency := TPngTransparencyFormat2.Create;
      if Assigned(OldTransparency) then
      begin
        FTransparency.Assign(OldTransparency);
        FreeAndNil(OldTransparency);
      end;
    end;
  ctIndexedColor:
    if not (FTransparency is TPngTransparencyFormat3) then
    begin
      FTransparency := TPngTransparencyFormat3.Create;
      if Assigned(OldTransparency) then
      begin
        FTransparency.Assign(OldTransparency);
        FreeAndNil(OldTransparency);
      end;
    end;
  else
    if Assigned(FTransparency) then
      FreeAndNil(FTransparency);
 end;
end;

function TChunkPngTransparency.GetChunkSize: Cardinal;
begin
  if Assigned(FTransparency) then
    Result := FTransparency.ChunkSize
  else
    Result := 0;
end;

procedure TChunkPngTransparency.ReadFromStream(Stream: TStream);
begin
  if Assigned(FTransparency) then
    FTransparency.ReadFromStream(Stream);
end;

procedure TChunkPngTransparency.WriteToStream(Stream: TStream);
begin
  // check consistency
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);
    ctTrueColor:
      if not (FTransparency is TPngTransparencyFormat2) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);
    ctIndexedColor:
      if not (FTransparency is TPngTransparencyFormat3) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);
  end;

  if Assigned(FTransparency) then
    FTransparency.WriteToStream(Stream);
end;


{ TPngTransparencyFormat0 }

procedure TPngTransparencyFormat0.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat0 then
    with TPngTransparencyFormat0(Dest) do
    begin
      FGraySampleValue := Self.FGraySampleValue;
    end
  else
    inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngTransparencyFormat0.ReadFromStream(Stream: TStream);
begin
  inherited;

  FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat0.WriteToStream(Stream: TStream);
begin
  inherited;

  WriteSwappedWord(Stream, FGraySampleValue);
end;


{ TPngTransparencyFormat2 }

procedure TPngTransparencyFormat2.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat2 then
    with TPngTransparencyFormat2(Dest) do
    begin
      FRedSampleValue := Self.FRedSampleValue;
      FBlueSampleValue := Self.FBlueSampleValue;
      FGreenSampleValue := Self.FGreenSampleValue;
    end
  else
    inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngTransparencyFormat2.ReadFromStream(Stream: TStream);
begin
  inherited;

  FRedSampleValue  := ReadSwappedWord(Stream);
  FBlueSampleValue  := ReadSwappedWord(Stream);
  FGreenSampleValue  := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat2.WriteToStream(Stream: TStream);
begin
  inherited;

  WriteSwappedWord(Stream, FRedSampleValue);
  WriteSwappedWord(Stream, FBlueSampleValue);
  WriteSwappedWord(Stream, FGreenSampleValue);
end;


{ TPngTransparencyFormat3 }

procedure TPngTransparencyFormat3.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat3 then
    with TPngTransparencyFormat3(Dest) do
    begin
      SetLength(FTransparency, Length(Self.FTransparency));
      Move(Self.FTransparency[0], FTransparency, Length(FTransparency));
    end
  else
    inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Cardinal;
begin
  Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Cardinal;
begin
  Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Cardinal): Byte;
begin
  if Index < Count then
    Result := FTransparency[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.ReadFromStream(Stream: TStream);
begin
  inherited;

  with Stream do
  begin
    SetLength(FTransparency, Size - Position);
    Read(FTransparency[0], Length(FTransparency));
  end;
end;

procedure TPngTransparencyFormat3.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FTransparency[0], Length(FTransparency));
end;


{ TChunkPngPhysicalPixelDimensions }

procedure TChunkPngPhysicalPixelDimensions.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPhysicalPixelDimensions then
    with TChunkPngPhysicalPixelDimensions(Dest) do
    begin
      FPixelsPerUnitX := Self.FPixelsPerUnitX;
      FPixelsPerUnitY := Self.FPixelsPerUnitY;
      FUnit := Self.FUnit;
    end
  else
    inherited;
end;

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
  Result := 'pHYs';
end;

function TChunkPngPhysicalPixelDimensions.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPhysicalPixelDimensions.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read pixels per unit, X axis
    FPixelsPerUnitX := ReadSwappedCardinal(Stream);

    // read pixels per unit, Y axis
    FPixelsPerUnitY := ReadSwappedCardinal(Stream);

    // read unit
    Read(FUnit, 1);
  end;
end;

procedure TChunkPngPhysicalPixelDimensions.WriteToStream(Stream: TStream);
begin
  with Stream do
  begin
    // write pixels per unit, X axis
    WriteSwappedCardinal(Stream, FPixelsPerUnitX);

    // write pixels per unit, Y axis
    WriteSwappedCardinal(Stream, FPixelsPerUnitY);

    // write unit
    Write(FUnit, 1);
  end;
end;


{ TChunkPngPhysicalScale }

procedure TChunkPngPhysicalScale.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPhysicalScale then
    with TChunkPngPhysicalScale(Dest) do
    begin
      FUnitSpecifier  := Self.FUnitSpecifier;
      FUnitsPerPixelX := Self.FUnitsPerPixelX;
      FUnitsPerPixelY := Self.FUnitsPerPixelY;
    end
  else
    inherited;
end;

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
  Result := 'sCAL';
end;

function TChunkPngPhysicalScale.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngPhysicalScale.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read unit specifier
    Read(FUnitSpecifier, 1);

    // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
  end;
end;

procedure TChunkPngPhysicalScale.WriteToStream(Stream: TStream);
begin
  raise EPngError.Create(RCStrNotYetImplemented);
  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;


{ TChunkPngImageOffset }

procedure TChunkPngImageOffset.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageOffset then
    with TChunkPngImageOffset(Dest) do
    begin
      FImagePositionX := Self.FImagePositionX;
      FImagePositionY := Self.FImagePositionY;
      FUnitSpecifier  := Self.FUnitSpecifier;
    end
  else
    inherited;
end;

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
  Result := 'oFFs';
end;

function TChunkPngImageOffset.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngImageOffset.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read image positions
    FImagePositionX := ReadSwappedCardinal(Stream);
    FImagePositionY := ReadSwappedCardinal(Stream);

    // read unit specifier
    Read(FUnitSpecifier, 1);
  end;
end;

procedure TChunkPngImageOffset.WriteToStream(Stream: TStream);
begin
  // write image positions
  WriteSwappedCardinal(Stream, FImagePositionX);
  WriteSwappedCardinal(Stream, FImagePositionY);

  // write unit specifier
  Write(FUnitSpecifier, 1);
end;


{ TChunkPngPixelCalibrator }

procedure TChunkPngPixelCalibrator.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPixelCalibrator then
    with TChunkPngPixelCalibrator(Dest) do
    begin
      FCalibratorName    := Self.FCalibratorName;
      FOriginalZeroes[0] := Self.FOriginalZeroes[0];
      FOriginalZeroes[1] := Self.FOriginalZeroes[1];
      FEquationType      := Self.FEquationType;
      FNumberOfParams    := Self.FNumberOfParams;
      FUnitName          := Self.FUnitName;
     end
  else
    inherited;
end;

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
  Result := 'pCAL';
end;

function TChunkPngPixelCalibrator.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPixelCalibrator.ReadFromStream(Stream: TStream);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
  with Stream do
  begin
    // read keyword
    Index := 1;
    SetLength(FCalibratorName, 80);
    while (Position < Size) do
    begin
      Read(FCalibratorName[Index], SizeOf(Byte));
      if FCalibratorName[Index] = #0 then
      begin
        SetLength(FCalibratorName, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // read original zeros
    FOriginalZeroes[0] := ReadSwappedCardinal(Stream);
    FOriginalZeroes[1] := ReadSwappedCardinal(Stream);

    // read equation type
    Stream.Read(FEquationType, 1);

    // read number of parameters
    Stream.Read(FNumberOfParams, 1);

    // read keyword
    Index := 1;
    SetLength(FUnitName, 80);
    while (Position < Size) do
    begin
      Read(FUnitName[Index], SizeOf(Byte));
      if FUnitName[Index] = #0 then
      begin
        SetLength(FUnitName, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    for ParamIndex := 0 to FNumberOfParams - 2 do
    begin
      // yet todo
    end;
  end;
end;

procedure TChunkPngPixelCalibrator.WriteToStream(Stream: TStream);
begin
  inherited;

end;


{ TCustomChunkPngText }

procedure TCustomChunkPngText.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunkPngText then
  with TCustomChunkPngText(Dest) do
   begin
    FKeyword := Self.FKeyword;
    FText    := Self.FText;
   end
 else inherited;
end;

procedure TCustomChunkPngText.KeywordChanged;
begin
  // yet empty
end;

procedure TCustomChunkPngText.TextChanged;
begin
  // yet empty
end;


{ TChunkPngText }

class function TChunkPngText.GetClassChunkName: TChunkName;
begin
  Result := 'tEXt';
end;

function TChunkPngText.GetChunkSize: Cardinal;
begin
  Result := Length(FKeyword) + Length(FText) + 1;
end;

procedure TChunkPngText.ReadFromStream(Stream: TStream);
var
  Index : Integer;
begin
  with Stream do
  begin
    // read keyword
    Index := 1;
    SetLength(FKeyword, 80);
    while (Position < Size) do
    begin
      Read(FKeyword[Index], SizeOf(Byte));
      if FKeyword[Index] = #0 then
      begin
        SetLength(FKeyword, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // read text
    Index := 1;
    SetLength(FText, Size - Position);
    while (Position < Size) do
    begin
      Read(FText[Index], SizeOf(Byte));
      Inc(Index);
    end;
  end;
end;

procedure TChunkPngText.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  with Stream do
  begin
    // write keyword
    Write(FKeyword[1], Length(FKeyword));

    // write separator
    Temp := 0;
    Write(Temp, 1);

    // write text
    Write(FText[1], Length(FText));
  end;
end;


{ TChunkPngCompressedText }

procedure TChunkPngCompressedText.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngCompressedText then
    with TChunkPngCompressedText(Dest) do
    begin
      FCompressionMethod := Self.FCompressionMethod;
    end
  else
    inherited;
end;

class function TChunkPngCompressedText.GetClassChunkName: TChunkName;
begin
  Result := 'zTXt';
end;

function TChunkPngCompressedText.GetChunkSize: Cardinal;
var
  OutputStream: TMemoryStream;
begin
  OutputStream := TMemoryStream.Create;
  try
    // compress text
    ZCompress(@FText[1], Length(FText), OutputStream);

    // calculate chunk size
    Result := Length(FKeyword) + OutputStream.Size + 1;
  finally
    FreeAndNil(OutputStream);
  end;
end;

procedure TChunkPngCompressedText.ReadFromStream(Stream: TStream);
var
  DataIn     : Pointer;
  DataInSize : Integer;
  Output     : TMemoryStream;
  Index      : Integer;
begin
  inherited;

  with Stream do
  begin
    // read keyword
    Index := 1;
    SetLength(FKeyword, 80);
    while (Position < Size) do
    begin
      Read(FKeyword[Index], SizeOf(Byte));
      if FKeyword[Index] = #0 then
      begin
        SetLength(FKeyword, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // read compression method
    Read(FCompressionMethod, SizeOf(Byte));

    // read text
    if FCompressionMethod = 0 then
    begin
      DataInSize := Size - Position;
      GetMem(DataIn, DataInSize);
      try
        Read(DataIn^, DataInSize);

        Output := TMemoryStream.Create;
        try
          ZDecompress(DataIn, DataInSize, Output);
          SetLength(FText, Output.Size);
          Move(Output.Memory^, FText[1], Output.Size);
        finally
          FreeAndNil(Output);
        end;
      finally
        FreeMem(DataIn);
      end;
    end;
  end;
end;

procedure TChunkPngCompressedText.WriteToStream(Stream: TStream);
var
  OutputStream: TMemoryStream;
  Temp         : Byte;
begin
  OutputStream := TMemoryStream.Create;
  try
    // compress text
    ZCompress(@FText[1], Length(FText), OutputStream);

    with Stream do
    begin
      // write keyword
      Write(FKeyword[1], Length(FKeyword));

      // write separator
      Temp := 0;
      Write(Temp, 1);

      // write text
      Write(FText[1], Length(FText));

      // write compression method
      Write(FCompressionMethod, SizeOf(Byte));

      // write text
      Write(OutputStream.Memory^, OutputStream.Size);
    end;
  finally
    FreeAndNil(OutputStream);
  end;
end;


{ TChunkPngInternationalText }

procedure TChunkPngInternationalText.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngInternationalText then
    with TChunkPngInternationalText(Dest) do
    begin
      FCompressionMethod := Self.FCompressionMethod;
      FCompressionFlag   := Self.FCompressionFlag;
      FLanguageString    := Self.FLanguageString;
      FTranslatedKeyword := Self.FTranslatedKeyword;
    end
  else
    inherited;
end;

class function TChunkPngInternationalText.GetClassChunkName: TChunkName;
begin
  Result := 'iTXt';
end;

function TChunkPngInternationalText.GetChunkSize: Cardinal;
begin
  Result := 0;
end;

procedure TChunkPngInternationalText.ReadFromStream(Stream: TStream);
var
  Index : Integer;
begin
  inherited;

  with Stream do
  begin
    // read keyword
    Index := 1;
    SetLength(FKeyword, 80);
    while (Position < Size) do
    begin
      Read(FKeyword[Index], SizeOf(Byte));
      if FKeyword[Index] = #0 then
      begin
        SetLength(FKeyword, Index - 1);
        Break;
      end;
     Inc(Index);
    end;

    // read compression flag
    Read(FCompressionFlag, SizeOf(Byte));

    // read compression method
    Read(FCompressionMethod, SizeOf(Byte));

    // read language string
    Index := 1;
    SetLength(FLanguageString, 10);
    while (Position < Size) do
    begin
      Read(FLanguageString[Index], SizeOf(Byte));
      if FLanguageString[Index] = #0 then
      begin
        SetLength(FLanguageString, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // yet todo!
    Exit;
  end;
end;

procedure TChunkPngInternationalText.WriteToStream(Stream: TStream);
begin
  // raise EPngError.Create(RCStrNotYetImplemented);
end;


{ TChunkPngImageData }

constructor TChunkPngImageData.Create;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TChunkPngImageData.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageData then
    with TChunkPngImageData(Dest) do
    begin
      FData.Seek(0, soFromBeginning);
      Self.FData.Seek(0, soFromBeginning);
      FData.CopyFrom(Self.FData, Self.FData.Size);
      FData.Seek(0, soFromBeginning);
    end
  else
    inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
  Result := 'IDAT';
end;

function TChunkPngImageData.GetChunkSize: Cardinal;
begin
  Result := FData.Size;
end;

procedure TChunkPngImageData.ReadFromStream(Stream: TStream);
begin
  inherited;

  FData.CopyFrom(Stream, Stream.Size - Stream.Position);
end;

procedure TChunkPngImageData.WriteToStream(Stream: TStream);
begin
  FData.Seek(0, soFromBeginning);
  Stream.CopyFrom(FData, FData.Size);
end;


{ TChunkPngTime }

procedure TChunkPngTime.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngTime then
    with TChunkPngTime(Dest) do
    begin
      FYear   := Self.FYear;
      FMonth  := Self.FMonth;
      FDay    := Self.FDay;
      FHour   := Self.FHour;
      FMinute := Self.FMinute;
      FSecond := Self.FSecond;
    end
  else
    inherited;
end;

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
  Result := 'tIME';
end;

function TChunkPngTime.GetModifiedDateTime: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

function TChunkPngTime.GetChunkSize: Cardinal;
begin
  Result := 7;
end;

procedure TChunkPngTime.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read year
    FYear := ReadSwappedWord(Stream);

    // read month
    Read(FMonth, SizeOf(Byte));

    // read day
    Read(FDay, SizeOf(Byte));

    // read hour
    Read(FHour, SizeOf(Byte));

    // read minute
    Read(FMinute, SizeOf(Byte));

    // read second
    Read(FSecond, SizeOf(Byte));
  end;
end;

procedure TChunkPngTime.WriteToStream(Stream: TStream);
begin
  with Stream do
  begin
    // write year
    WriteSwappedWord(Stream, FYear);

    // write month
    Write(FMonth, SizeOf(Byte));

    // write day
    Write(FDay, SizeOf(Byte));

    // write hour
    Write(FHour, SizeOf(Byte));

    // write minute
    Write(FMinute, SizeOf(Byte));

    // write second
    Write(FSecond, SizeOf(Byte));
  end;
end;


{ TChunkPngEmbeddedIccProfile }

procedure TChunkPngEmbeddedIccProfile.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngEmbeddedIccProfile then
    with TChunkPngEmbeddedIccProfile(Dest) do
    begin
      FProfileName       := Self.FProfileName;
      FCompressionMethod := Self.FCompressionMethod;
    end
  else
    inherited;
end;

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
  Result := 'iCCP';
end;

function TChunkPngEmbeddedIccProfile.GetChunkSize: Cardinal;
begin
  Result := Length(FProfileName) + 2;
end;

procedure TChunkPngEmbeddedIccProfile.ReadFromStream(Stream: TStream);
var
  Index : Integer;
begin
  with Stream do
  begin
    // read keyword
    Index := 1;
    SetLength(FProfileName, 80);
    while (Position < Size) do
    begin
      Read(FProfileName[Index], SizeOf(Byte));
      if FProfileName[Index] = #0 then
      begin
        SetLength(FProfileName, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // read compression method
    Read(FCompressionMethod, 1);

    // not yet completed
  end;
end;

procedure TChunkPngEmbeddedIccProfile.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  with Stream do
  begin
    // write keyword
    Write(FProfileName[1], Length(FProfileName));

    // write separator
    Temp := 0;
    Write(Temp, 1);

    // write compression method
    Write(FCompressionMethod, 1);
  end;
end;


{ TChunkPngGamma }

procedure TChunkPngGamma.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngGamma then
    with TChunkPngGamma(Dest) do
    begin
      FGamma := Self.FGamma;
    end
  else
    inherited;
end;

class function TChunkPngGamma.GetClassChunkName: TChunkName;
begin
  Result := 'gAMA';
end;

function TChunkPngGamma.GetGammaAsSingle: Single;
begin
  Result := FGamma * 1E-5;
end;

function TChunkPngGamma.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngGamma.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read gamma
    FGamma := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngGamma.WriteToStream(Stream: TStream);
begin
  with Stream do
  begin
    // write gamma
    WriteSwappedCardinal(Stream, FGamma);
  end;
end;


{ TChunkPngStandardColorSpaceRGB }

procedure TChunkPngStandardColorSpaceRGB.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngStandardColorSpaceRGB then
    with TChunkPngStandardColorSpaceRGB(Dest) do
    begin
      FRenderingIntent := Self.FRenderingIntent;
    end
  else
    inherited;
end;

class function TChunkPngStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
  Result := 'sRGB';
end;

function TChunkPngStandardColorSpaceRGB.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TChunkPngStandardColorSpaceRGB.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read rendering intent
    Read(FRenderingIntent, SizeOf(Byte));
  end;
end;

procedure TChunkPngStandardColorSpaceRGB.WriteToStream(Stream: TStream);
begin
  // write rendering intent
  Stream.Write(FRenderingIntent, SizeOf(Byte));
end;


{ TChunkPngPrimaryChromaticities }

class function TChunkPngPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
  Result := 'cHRM';
end;

procedure TChunkPngPrimaryChromaticities.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPrimaryChromaticities then
    with TChunkPngPrimaryChromaticities(Dest) do
    begin
      FWhiteX := Self.FWhiteX;
      FWhiteY := Self.FWhiteY;
      FRedX   := Self.FRedX;
      FRedY   := Self.FRedY;
      FGreenX := Self.FGreenX;
      FGreenY := Self.FGreenY;
      FBlueX  := Self.FBlueX;
      FBlueY  := Self.FBlueY;
    end
  else
    inherited;
end;

function TChunkPngPrimaryChromaticities.GetBlueX: Single;
begin
  Result := FBlueX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: Single;
begin
  Result := FBlueY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: Single;
begin
  Result := FGreenX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: Single;
begin
  Result := FGreenY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedX: Single;
begin
  Result := FRedX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedY: Single;
begin
  Result := FRedY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: Single;
begin
  Result := FWhiteX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: Single;
begin
  Result := FWhiteY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetChunkSize: Cardinal;
begin
  Result := 32;
end;

procedure TChunkPngPrimaryChromaticities.ReadFromStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read white point x
    FWhiteX := ReadSwappedCardinal(Stream);

    // read white point y
    FWhiteY := ReadSwappedCardinal(Stream);

    // read red x
    FRedX := ReadSwappedCardinal(Stream);

    // read red y
    FRedY := ReadSwappedCardinal(Stream);

    // read green x
    FGreenX := ReadSwappedCardinal(Stream);

    // read green y
    FGreenY := ReadSwappedCardinal(Stream);

    // read blue x
    FBlueX := ReadSwappedCardinal(Stream);

    // read blue y
    FBlueY := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngPrimaryChromaticities.WriteToStream(Stream: TStream);
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // write white point x
    WriteSwappedCardinal(Stream, FWhiteX);

    // write white point y
    WriteSwappedCardinal(Stream, FWhiteY);

    // write red x
    WriteSwappedCardinal(Stream, FRedX);

    // write red y
    WriteSwappedCardinal(Stream, FRedY);

    // write green x
    WriteSwappedCardinal(Stream, FGreenX);

    // write green y
    WriteSwappedCardinal(Stream, FGreenY);

    // write blue x
    WriteSwappedCardinal(Stream, FBlueX);

    // write blue y
    WriteSwappedCardinal(Stream, FBlueY);
  end;
end;


{ TPngSignificantBitsFormat0 }

constructor TPngSignificantBitsFormat0.Create(BitDepth: Integer = 8);
begin
  inherited;
  FGrayBits := BitDepth;
end;

procedure TPngSignificantBitsFormat0.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
    begin
      FGrayBits := Self.FGrayBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngSignificantBitsFormat0.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
end;


{ TPngSignificantBitsFormat23 }

constructor TPngSignificantBitsFormat23.Create(BitDepth: Integer = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
end;

procedure TPngSignificantBitsFormat23.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Cardinal;
begin
  Result := 3;
end;

procedure TPngSignificantBitsFormat23.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
end;


{ TPngSignificantBitsFormat4 }

constructor TPngSignificantBitsFormat4.Create(BitDepth: Integer = 8);
begin
  inherited;
  FGrayBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat4.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat4 then
    with TPngSignificantBitsFormat4(Dest) do
    begin
      FGrayBits  := Self.FGrayBits;
      FAlphaBits := Self.FAlphaBits;
    end
  else if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
      FGrayBits  := Self.FGrayBits
  else
    inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngSignificantBitsFormat4.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


{ TPngSignificantBitsFormat6 }

constructor TPngSignificantBitsFormat6.Create(BitDepth: Integer = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat6.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat6 then
    with TPngSignificantBitsFormat6(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
      FAlphaBits := Self.FAlphaBits;
    end
  else if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngSignificantBitsFormat6.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


{ TChunkPngSignificantBits }

procedure TChunkPngSignificantBits.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngSignificantBits then
    with TChunkPngSignificantBits(Dest) do
    begin
      FSignificantBits.Assign(Self.FSignificantBits);
    end
  else
    inherited;
end;

constructor TChunkPngSignificantBits.Create(Header: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale:
      FSignificantBits := TPngSignificantBitsFormat0.Create(Header.BitDepth);
    ctTrueColor,
    ctIndexedColor:
      FSignificantBits := TPngSignificantBitsFormat23.Create(Header.BitDepth);
    ctGrayscaleAlpha:
      FSignificantBits := TPngSignificantBitsFormat4.Create(Header.BitDepth);
    ctTrueColorAlpha:
      FSignificantBits := TPngSignificantBitsFormat6.Create(Header.BitDepth);
  end;
end;

destructor TChunkPngSignificantBits.Destroy;
begin
  if Assigned(FSignificantBits) then
    FreeAndNil(FSignificantBits);

  inherited;
end;

class function TChunkPngSignificantBits.GetClassChunkName: TChunkName;
begin
  Result := 'sBIT';
end;

procedure TChunkPngSignificantBits.HeaderChanged;
var
  OldSignificantBits : TCustomPngSignificantBits;
begin
  inherited;

  // store old SignificantBits object
  OldSignificantBits := FSignificantBits;

  // change SignificantBits object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FSignificantBits is TPngSignificantBitsFormat0) then
      begin
        FSignificantBits := TPngSignificantBitsFormat0.Create(FHeader.BitDepth);
        if Assigned(OldSignificantBits) then
        begin
          FSignificantBits.Assign(OldSignificantBits);
          FreeAndNil(OldSignificantBits);
        end;
      end;
    ctTrueColor, ctIndexedColor:
      if not (FSignificantBits is TPngSignificantBitsFormat23) then
      begin
        FSignificantBits := TPngSignificantBitsFormat23.Create(FHeader.BitDepth);
        if Assigned(OldSignificantBits) then
        begin
          FSignificantBits.Assign(OldSignificantBits);
          FreeAndNil(OldSignificantBits);
        end;
      end;
    ctTrueColorAlpha:
      if not (FSignificantBits is TPngSignificantBitsFormat4) then
      begin
        FSignificantBits := TPngSignificantBitsFormat4.Create(FHeader.BitDepth);
        if Assigned(OldSignificantBits) then
        begin
          FSignificantBits.Assign(OldSignificantBits);
          FreeAndNil(OldSignificantBits);
        end;
      end;
    ctGrayscaleAlpha :
      if not (FSignificantBits is TPngSignificantBitsFormat6) then
      begin
        FSignificantBits := TPngSignificantBitsFormat6.Create(FHeader.BitDepth);
        if Assigned(OldSignificantBits) then
        begin
          FSignificantBits.Assign(OldSignificantBits);
          FreeAndNil(OldSignificantBits);
        end;
      end;
    else
      if Assigned(FSignificantBits) then
        FreeAndNil(FSignificantBits);
  end;
end;

function TChunkPngSignificantBits.GetChunkSize: Cardinal;
begin
  if Assigned(FSignificantBits) then
    Result := FSignificantBits.GetChunkSize
  else
    Result := 0;
end;

procedure TChunkPngSignificantBits.ReadFromStream(Stream: TStream);
begin
  if Assigned(FSignificantBits) then
  begin
    if Stream.Size < FSignificantBits.ChunkSize then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    FSignificantBits.ReadFromStream(Stream);
  end;
end;

procedure TChunkPngSignificantBits.WriteToStream(Stream: TStream);
begin
  if Assigned(FSignificantBits) then
    FSignificantBits.WriteToStream(Stream);
end;


{ TPngBackgroundColorFormat04 }

procedure TPngBackgroundColorFormat04.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat04 then
    with TPngBackgroundColorFormat04(Dest) do
    begin
      FGraySampleValue := Self.FGraySampleValue;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngBackgroundColorFormat04.ReadFromStream(Stream: TStream);
begin
  FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat04.WriteToStream(Stream: TStream);
begin
  WriteSwappedWord(Stream, FGraySampleValue);
end;


{ TPngBackgroundColorFormat26 }

procedure TPngBackgroundColorFormat26.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat26 then
    with TPngBackgroundColorFormat26(Dest) do
    begin
      FRedSampleValue := Self.FRedSampleValue;
      FBlueSampleValue := Self.FBlueSampleValue;
      FGreenSampleValue := Self.FGreenSampleValue;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngBackgroundColorFormat26.ReadFromStream(Stream: TStream);
begin
  FRedSampleValue := ReadSwappedWord(Stream);
  FGreenSampleValue := ReadSwappedWord(Stream);
  FBlueSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat26.WriteToStream(Stream: TStream);
begin
  WriteSwappedWord(Stream, FRedSampleValue);
  WriteSwappedWord(Stream, FGreenSampleValue);
  WriteSwappedWord(Stream, FBlueSampleValue);
end;


{ TPngBackgroundColorFormat3 }

procedure TPngBackgroundColorFormat3.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat3 then
    with TPngBackgroundColorFormat3(Dest) do
    begin
      FIndex := Self.FIndex;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngBackgroundColorFormat3.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.WriteToStream(Stream: TStream);
begin
  Stream.Write(FIndex, 1);
end;


{ TChunkPngBackgroundColor }

procedure TChunkPngBackgroundColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngBackgroundColor then
    with TChunkPngBackgroundColor(Dest) do
    begin
      FBackground.Assign(Self.FBackground);
    end
  else
    inherited;
end;

constructor TChunkPngBackgroundColor.Create(Header: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      FBackground := TPngBackgroundColorFormat04.Create;
    ctTrueColor, ctTrueColorAlpha:
      FBackground := TPngBackgroundColorFormat26.Create;
    ctIndexedColor:
      FBackground := TPngBackgroundColorFormat3.Create;
  end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);
  inherited;
end;

class function TChunkPngBackgroundColor.GetClassChunkName: TChunkName;
begin
  Result := 'bKGD';
end;

procedure TChunkPngBackgroundColor.HeaderChanged;
var
  OldBackground : TCustomPngBackgroundColor;
begin
  inherited;

  // store old background object
  OldBackground := FBackground;

  // change background object class
  case FHeader.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      if not (FBackground is TPngBackgroundColorFormat04) then
      begin
        FBackground := TPngBackgroundColorFormat04.Create;
        if Assigned(OldBackground) then
        begin
          FBackground.Assign(OldBackground);
          FreeAndNil(OldBackground);
        end;
      end;
    ctTrueColor, ctTrueColorAlpha :
      if not (FBackground is TPngBackgroundColorFormat26) then
      begin
        FBackground := TPngBackgroundColorFormat26.Create;
        if Assigned(OldBackground) then
        begin
          FBackground.Assign(OldBackground);
          FreeAndNil(OldBackground);
        end;
      end;
    ctIndexedColor :
      if not (FBackground is TPngBackgroundColorFormat3) then
      begin
        FBackground := TPngBackgroundColorFormat3.Create;
        if Assigned(OldBackground) then
        begin
          FBackground.Assign(OldBackground);
          FreeAndNil(OldBackground);
        end;
      end;
    else
      if Assigned(FBackground) then
        FreeAndNil(FBackground);
  end;
end;

function TChunkPngBackgroundColor.GetChunkSize: Cardinal;
begin
  if Assigned(FBackground) then
    Result := FBackground.GetChunkSize
  else
    Result := 0;
end;

procedure TChunkPngBackgroundColor.ReadFromStream(Stream: TStream);
begin
  if Assigned(FBackground) then
  begin
    if Stream.Size < FBackground.ChunkSize then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    FBackground.ReadFromStream(Stream);
  end;
end;

procedure TChunkPngBackgroundColor.WriteToStream(Stream: TStream);
begin
  if Assigned(FBackground) then
    FBackground.WriteToStream(Stream);
end;


{ TChunkPngImageHistogram }

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
  Result := 'hIST';
end;

function TChunkPngImageHistogram.GetCount: Cardinal;
begin
  Result := Length(FHistogram);
end;

function TChunkPngImageHistogram.GetFrequency(Index: Cardinal): Word;
begin
  if Index < Count then
    Result := FHistogram[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TChunkPngImageHistogram.GetChunkSize: Cardinal;
begin
  Result := Count * SizeOf(Word);
end;

procedure TChunkPngImageHistogram.ReadFromStream(Stream: TStream);
var
  Index : Integer;
begin
  // check size
  if (ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // adjust histogram array size
  SetLength(FHistogram, ChunkSize div 2);

  // read histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    FHistogram[Index] := ReadSwappedWord(Stream);
end;

procedure TChunkPngImageHistogram.WriteToStream(Stream: TStream);
var
  Index : Integer;
begin
  // write histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    WriteSwappedWord(Stream, FHistogram[Index]);
end;


{ TChunkPngSuggestedPalette }

constructor TChunkPngSuggestedPalette.Create(Header: TChunkPngImageHeader);
begin
  inherited;
  FData := nil;
  FCount := 0;
end;

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin
  Result := 'sPLT';
end;

function TChunkPngSuggestedPalette.GetCount: Cardinal;
begin
  Result := FCount;
end;

function TChunkPngSuggestedPalette.GetChunkSize: Cardinal;
begin
  Result := Cardinal(Length(FPaletteName)) + 2 +
    (4 * (FSampleDepth shr 3) + 2) * Count;
end;

procedure TChunkPngSuggestedPalette.ReadFromStream(Stream: TStream);
var
  Index      : Integer;
  DataSize   : Integer;
begin
  with Stream do
  begin
    if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

    // read palette name
    Index := 1;
    SetLength(FPaletteName, 80);
    while (Position < ChunkSize) do
    begin
      Read(FPaletteName[Index], SizeOf(Byte));
      if FPaletteName[Index] = #0 then
      begin
        SetLength(FPaletteName, Index - 1);
        Break;
      end;
      Inc(Index);
    end;

    // read sample depth
    Read(FSampleDepth, 1);

    DataSize := Integer(ChunkSize) - Length(FPaletteName) - 2;
    Assert(DataSize >= 0);
    Assert(DataSize mod 2 = 0);
    Assert(DataSize mod (4 * (FSampleDepth shr 3) + 2) = 0);
    FCount := DataSize div (4 * (FSampleDepth shr 3) + 2);
    ReallocMem(FData, DataSize);

    if FSampleDepth = 8 then
      for Index := 0 to FCount - 1 do
        with PSuggestedPalette8ByteArray(FData)^[Index] do
        begin
          Read(Red, 1);
          Read(Green, 1);
          Read(Blue, 1);
          Read(Alpha, 1);
          Frequency := ReadSwappedWord(Stream);
        end
    else if FSampleDepth = 16 then
      for Index := 0 to FCount - 1 do
        with PSuggestedPalette16ByteArray(FData)^[Index] do
        begin
          Red := ReadSwappedWord(Stream);
          Green := ReadSwappedWord(Stream);
          Blue := ReadSwappedWord(Stream);
          Alpha := ReadSwappedWord(Stream);
          Frequency := ReadSwappedWord(Stream);
        end;
  end;
end;

procedure TChunkPngSuggestedPalette.WriteToStream(Stream: TStream);
begin
  raise EPngError.Create(RCStrNotYetImplemented);

  // yet todo
end;


{ TChunkList }

destructor TChunkList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TChunkList.Add(Item: TCustomChunk);
begin
  SetLength(FChunks, Length(FChunks) + 1);
  FChunks[Length(FChunks) - 1] := Item;
end;

procedure TChunkList.AssignTo(Dest: TPersistent);
var
  Index      : Integer;
  ChunkClass : TCustomDefinedChunkWithHeaderClass;
begin
  if Dest is TChunkList then
    with TChunkList(Dest) do
    begin
      Clear;
      SetLength(FChunks, Self.Count);
      for Index := 0 to Self.Count - 1 do
        if Self.FChunks[Index] is TCustomDefinedChunkWithHeader then
        begin
          ChunkClass := TCustomDefinedChunkWithHeaderClass(Self.FChunks[Index].ClassType);
          FChunks[Index] := ChunkClass.Create(TCustomDefinedChunkWithHeader(Self.FChunks[Index]).FHeader);
          FChunks[Index].Assign(Self.FChunks[Index]);
        end
        else
          inherited;
    end
    else
      inherited;
end;

procedure TChunkList.Clear;
var
  Index : Integer;
begin
  for Index := 0 to Count - 1 do
    FreeAndNil(FChunks[Index]);
  SetLength(FChunks, 0)
end;

procedure TChunkList.Delete(Index: Cardinal);
begin
  if Index >= Count then
    raise EPngError.Create(RCStrEmptyChunkList);
  FreeAndNil(FChunks[Index]);
  if Index < Count then
    System.Move(FChunks[Index + 1], FChunks[Index], (Count - Index) * SizeOf(Pointer));
  SetLength(FChunks, Length(FChunks) - 1);
end;

function TChunkList.GetChunk(Index: Integer): TCustomChunk;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    Result := FChunks[Index];
end;

function TChunkList.GetCount: Cardinal;
begin
  Result := Length(FChunks);
end;

function TChunkList.IndexOf(Item: TCustomChunk): Integer;
begin
  for Result := 0 to Count - 1 do
    if FChunks[Result] = Item then
      Exit;
  Result := -1;
end;

procedure TChunkList.Remove(Item: TCustomChunk);
begin
  Delete(IndexOf(Item));
end;


{ TCustomPngCoder }

constructor TCustomPngCoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil; Transparency : TCustomPngTransparency = nil);
begin
  FStream       := Stream;
  FHeader       := Header;
  FGamma        := Gamma;
  FPalette      := Palette;
  FTransparency := Transparency;
  FMappingTable := nil;
  FAlphaTable   := nil;
  BuildMappingTables;
  inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
  Dispose(FMappingTable);
  Dispose(FAlphaTable);
  inherited;
end;

procedure TCustomPngCoder.BuildMappingTables;
var
  Index        : Integer;
  Palette      : PRGB24Array;
  FracVal      : Single;
  Color        : TRGB24;
  MaxByte      : Byte;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
  if FHeader.HasPalette then
  begin
    if Assigned(FPalette) then
    begin
      GetMem(FMappingTable, FPalette.Count * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);

      if Assigned(FGamma) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Color := FPalette.PaletteEntry[Index];
          Palette[Index].R := Round(Power((Color.R * COne255th), PreCalcGamma) * 255);
          Palette[Index].G := Round(Power((Color.G * COne255th), PreCalcGamma) * 255);
          Palette[Index].B := Round(Power((Color.B * COne255th), PreCalcGamma) * 255);
        end;
      end
      else
        for Index := 0 to FPalette.Count - 1 do
          Palette[Index] := FPalette.PaletteEntry[Index];
    end
    else
    begin
      // create gray scale palette
      GetMem(FMappingTable, 256 * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);
      MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
      FracVal := 1 / MaxByte;

      if Assigned(FGamma) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Palette[Index].R := Round(Power(Index * FracVal, PreCalcGamma) * 255);
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].B;
        end;
      end
      else
      begin
        for Index := 0 to MaxByte do
        begin
          Palette[Index].R := Round(255 * (Index * FracVal));
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].R;
        end;
      end;
    end;

   // build alpha table
   GetMem(FAlphaTable, 256);
   FillChar(FAlphaTable^, 256, $FF);

   // eventually fill alpha table
   if FTransparency is TPngTransparencyFormat3 then
     with TPngTransparencyFormat3(FTransparency) do
       for Index := 0 to Count - 1 do
         FAlphaTable[Index] := Transparency[Index];
  end
  else
  begin
    GetMem(FMappingTable, 256);
    if Assigned(FGamma) and (FGamma.Gamma <> 0) then
    begin
      PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
      for Index := 0 to $FF do
        FMappingTable[Index] := Round(Power((Index * COne255th), PreCalcGamma) * 255);
    end
    else
      for Index := 0 to $FF do
        FMappingTable[Index] := Index;
  end;
end;

procedure TCustomPngCoder.DecodeFilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
  // RCX = Self
  // RDX = CurrentRow
  // R9  = BytesPerRow
        ADD     RDX, 1
        MOV     RAX, RDX
        MOV     RCX, BytesPerRow
        ADD     RAX, PixelByteSize
        SUB     RCX, PixelByteSize
        LEA     RAX, RAX + RCX
        LEA     RDX, RDX + RCX
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        ADD     EDX, 1
        MOV     EAX, EDX
        MOV     ECX, BytesPerRow.DWORD
        ADD     EAX, PixelByteSize.DWORD
        SUB     ECX, PixelByteSize.DWORD
        LEA     EAX, EAX + ECX
        LEA     EDX, EDX + ECX
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
        // RCX = Self
        // RDX = CurrentRow
        // R8  = PreviousRow
        // R9  = BytesPerRow
        MOV     RAX, RDX
        MOV     RDX, R8
        MOV     RCX, BytesPerRow
        LEA     RAX, RAX + RCX + 1
        LEA     RDX, RDX + RCX + 1
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        MOV     EAX, EDX
        MOV     EDX, ECX
        MOV     ECX, BytesPerRow.DWORD
        LEA     EAX, EAX + ECX + 1
        LEA     EDX, EDX + ECX + 1
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

function PaethPredictor(a, b, c: Byte): Integer; {$IFNDEF TARGET_x64} pascal; {$ENDIF}
{$IFDEF PUREPASCAL}
var
  DistA, DistB, DistC: Integer;
begin
  DistA := Abs(b - c);
  DistB := Abs(a - c);
  DistC := Abs(a + b - c * 2);

  if (DistA <= DistB) and (DistA <= DistC) then Result := a else
  if DistB <= DistC then
    Result := b
  else
    Result := c;
{$ELSE}
asm
{$IFDEF TARGET_x64}
        // RCX = a
        // RDX = b
        // R8  = c

        // calculate DistA = Abs(b - c)
        MOVZX   RAX, b           // RAX = b
        SUB     RAX, R8          // RAX = b - c
        MOV     R10, RAX         // R10 = b - c
        JAE     @PositiveDistA   // if  R10 >= 0 then
        NOT     RAX              //   ...
        INC     RAX              //   RAX = Abs(b - c) = DistA

        @PositiveDistA:

        // calculate DistB = Abs(a - c)
        MOVZX   R11, a           // R11 = a
        SUB     R11, R8          // R11 = a - c
        MOV     R9, R11          // R9 = a - c
        JAE     @PositiveDistB   // if  R9 >= 0 then
        NOT     R11              //   ...
        INC     R11              //   R11 = Abs(a - c) = DistB

        @PositiveDistB:

        // calculate DistC = Abs(a + b - c * 2)
        ADD     R10, R9          // R10 = b - c + a - c = a + b - 2 * c
        JNL     @PositiveDistC   // if R10 >= 0 then
        NOT     R10              //   ...
        INC     R10              //   R10 = Abs(a + b - c * 2) = DistC

        @PositiveDistC:

        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R11          // R9 = DistA - DistB
        JA      @NextCheck       // if (DistA <= DistB) then
        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R10          // R9 = DistA - DistC
        JA      @NextCheck       // if (DistA <= DistC) then

        MOV     RAX, RCX         // RAX = a
        JMP     @Done            // Exit

        @NextCheck:
        MOV     R9, R11          // R9 = DistB
        SUB     R9, R10          // R9 = DistB - DistC
        JA      @ResultC         // if (DistB <= DistC) then

        MOV     RAX, RDX         // RAX = b
        JMP     @Done

        @ResultC:
        MOV     RAX, R8          // RAX = c

        @Done:
{$ELSE}
        MOVZX   EDX, c
        PUSH    EBX
        MOVZX   EAX, b
        SUB     EAX, EDX
        JAE     @PositiveDistA
        NOT     EAX
        INC     EAX

@PositiveDistA:
        MOVZX   EBX, a
        SUB     EBX, EDX
        JAE     @PositiveDistB
        NOT     EBX
        INC     EBX

@PositiveDistB:
        MOVZX   ECX, a
        SUB     ECX, EDX
        MOVZX   EDX, b
        ADD     ECX, EDX
        MOVZX   EDX, c
        SUB     ECX, EDX
        JAE     @PositiveDistC
        NOT     ECX
        INC     ECX

@PositiveDistC:
        MOV     EDX, EAX
        SUB     EDX, EBX
        JA      @NextCheck
        MOV     EDX, EAX
        SUB     EDX, ECX
        JA      @NextCheck

        MOVZX   EDX, a
        MOV     Result, EDX
        JMP     @Done

@NextCheck:
        MOV     EDX, EBX
        SUB     EDX, ECX
        JA      @ResultC

        MOVZX   EDX, b
        MOV     Result, EDX
        JMP     @Done

@ResultC:
        MOVZX   EDX, c
        MOV     Result, EDX

@Done:
        POP     EBX
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  DecodeFilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  // copy first pixel
  Move(CurrentRow[1], OutputRow[1], PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  EncodeFilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] -
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;


{ TFilePng }

constructor TFilePng.Create;
begin
  FImageHeader         := TChunkPngImageHeader.Create;
  FDataChunkList       := TChunkList.Create;
  FAdditionalChunkList := TChunkList.Create;

  FCompressionLevel    := Z_BEST_COMPRESSION;
  inherited;

  FChunkList.OwnsObjects := False;
end;

destructor TFilePng.Destroy;
begin
  FAdditionalChunkList.Clear;

  FreeAndNil(FAdditionalChunkList);
  FreeAndNil(FDataChunkList);
  FreeAndNil(FImageHeader);

  // free palette chunk
  if Assigned(FPaletteChunk) then
    FreeAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
    FreeAndNil(FGammaChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
    FreeAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
    FreeAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
    FreeAndNil(FPhysicalDimensions);

  // free primary chromaticities chunk
  if Assigned(FChromaChunk) then
    FreeAndNil(FChromaChunk);

  // free transparency chunk
  if Assigned(FTransparencyChunk) then
    FreeAndNil(FTransparencyChunk);

  // free transparency chunk
  if Assigned(FBackgroundChunk) then
    FreeAndNil(FBackgroundChunk);

  inherited;
end;

procedure TFilePng.CopyImageData(Stream: TStream);
var
  DataIndex   : Integer;
begin
  // combine all data chunks first
  for DataIndex := 0 to FDataChunkList.Count - 1 do
  begin
    // make sure the chunk is inded an image data chunk
    Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

    // concat current chunk to data stream
    with TChunkPngImageData(FDataChunkList[DataIndex]) do
    begin
      Data.Seek(0, soFromBeginning);
      Stream.CopyFrom(Data, Data.Size);
    end;
  end;
end;

procedure TFilePng.StoreImageData(Stream: TStream);
var
  DataChunk : TChunkPngImageData;
  ChunkSize : Integer;
begin
  // delete old image data
  FDataChunkList.Clear;

  ChunkSize := Stream.Size;
  while Stream.Position < Stream.Size do
  begin
    DataChunk := TChunkPngImageData.Create(ImageHeader);

    if (Stream.Size - Stream.Position) < ChunkSize then
      ChunkSize := (Stream.Size - Stream.Position);

    // copy data to IDAT chunk
    DataChunk.Data.CopyFrom(Stream, ChunkSize);

    // add data chunk to data chunk list
    FDataChunkList.Add(DataChunk);
  end;
end;

procedure TFilePng.DecompressImageDataToStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // copy image data from all data chunks to one continous data stream
    CopyImageData(DataStream);

    // check whether compression method is supported
    if FImageHeader.CompressionMethod <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressionMethod);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // decompress z-stream
    ZDecompress(DataStream, Stream);
  finally
    FreeAndNil(DataStream);
  end;
end;

procedure TFilePng.CompressImageDataFromStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // set compression method
    FImageHeader.CompressionMethod := 0;

    // compress Stream to DataStream
    if Stream is TMemoryStream then
      ZCompress(TMemoryStream(Stream), DataStream, FCompressionLevel)
    else
      raise EPngError.Create(RCStrNotYetImplemented);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // copy image data from all data chunks to one continous data stream
    StoreImageData(DataStream);
  finally
    FreeAndNil(DataStream);
  end;
end;

class function TFilePng.CanLoad(Stream: TStream): Boolean;
var
  ChunkName: TChunkName;
begin
  Result := Stream.Size >= 4;

  if Result then
  begin
    Stream.Read(ChunkName, 4);
    Stream.Seek(-4, soFromCurrent);
    Result := ChunkName = GetClassChunkName;
  end;
end;

procedure TFilePng.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Integer;
  ChunkCRC     : Cardinal;
  ChunkClass   : TCustomDefinedChunkWithHeaderClass;
  Chunk        : TCustomDefinedChunkWithHeader;
  MemoryStream : TMemoryStream;
begin
  with Stream do
  begin
    Clear;

    // check for minimum file size
    if Size < 8 then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read chunk ID
    Read(ChunkName, 4);
    if ChunkName <> GetClassChunkName then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read PNG magic
    Read(ChunkName, 4);
    if ChunkName <> CPngMagic then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    MemoryStream := TMemoryStream.Create;
    try
      // read image header chunk size
      ChunkSize := ReadSwappedCardinal(Stream);
      if ChunkSize > Stream.Size - 12 then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // read image header chunk ID
      Read(ChunkName, 4);
      if ChunkName <> 'IHDR' then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // reset position to the chunk start and copy stream to memory
      Seek(-4, soCurrent);
      MemoryStream.CopyFrom(Stream, ChunkSize + 4);
      MemoryStream.Seek(4, soFromBeginning);

      // load image header
      FImageHeader.ReadFromStream(MemoryStream);
      AddChunk(FImageHeader);

      // read image header chunk size
      ChunkCRC := 0;
      Read(ChunkCRC, 4);
      {$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
        raise EPngError.Create(RCStrCRCError);
      {$ENDIF}

      while Stream.Position < Stream.Size do
      begin
        // read image header chunk size
        ChunkSize := ReadSwappedCardinal(Stream);
        if ChunkSize > Stream.Size - Stream.Position - 4 then
          raise EPngError.Create(RCStrNotAValidPNGFile);

        // read chunk ID
        Read(ChunkName, 4);

        // check for stream end
        if ChunkName = 'IEND' then
        begin
          // read image header chunk size
          Read(ChunkCRC, 4);

          {$IFDEF CheckCRC}
          if ChunkCRC <> 2187346606 then
            raise EPngError.Create(RCStrCRCError);
          {$ENDIF}

          Break;
        end;

        // reset position to the chunk start and copy stream to memory
        Seek(-4, soCurrent);
        MemoryStream.Clear;
        MemoryStream.CopyFrom(Stream, ChunkSize + 4);

        // reset memory stream to beginning of the chunk
        MemoryStream.Seek(4, soFromBeginning);

        if ChunkName = 'IHDR' then
          raise EPngError.Create(RCStrNotAValidPNGFile)
        else if ChunkName = 'IDAT' then
          ReadImageDataChunk(MemoryStream, ChunkSize)
        else if ChunkName = 'gAMA' then
        begin
          if Assigned(FGammaChunk)
           then raise EPngError.Create(RCStrSeveralGammaChunks);
          FGammaChunk := TChunkPngGamma.Create(FImageHeader);
          FGammaChunk.ReadFromStream(MemoryStream);
          AddChunk(FGammaChunk);
        end
        else if ChunkName = 'cHRM' then
        begin
          if Assigned(FChromaChunk) then
            raise EPngError.Create(RCStrSeveralChromaChunks);
          FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
          FChromaChunk.ReadFromStream(MemoryStream);
          AddChunk(FChromaChunk);
        end
        else if ChunkName = 'tIME' then
        begin
          if Assigned(FTimeChunk) then
            raise EPngError.Create(RCStrSeveralTimeChunks);
          FTimeChunk := TChunkPngTime.Create(FImageHeader);
          FTimeChunk.ReadFromStream(MemoryStream);
          AddChunk(FTimeChunk);
        end
        else if ChunkName = 'sBIT' then
        begin
          if Assigned(FSignificantBits) then
            raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
          FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
          FSignificantBits.ReadFromStream(MemoryStream);
          AddChunk(FSignificantBits);
        end
        else if ChunkName = 'pHYs' then
        begin
          if Assigned(FPhysicalDimensions) then
            raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
          FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
          FPhysicalDimensions.ReadFromStream(MemoryStream);
          AddChunk(FPhysicalDimensions);
        end
        else if ChunkName = 'PLTE' then
        begin
          if Assigned(FPaletteChunk) then
            raise EPngError.Create(RCStrSeveralPaletteChunks);
          FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
          FPaletteChunk.ReadFromStream(MemoryStream);
          AddChunk(FPaletteChunk);
        end
        else if ChunkName = 'tRNS' then
        begin
          if Assigned(FTransparencyChunk) then
            raise EPngError.Create(RCStrSeveralTransparencyChunks);
          FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
          FTransparencyChunk.ReadFromStream(MemoryStream);
          AddChunk(FTransparencyChunk);
        end
        else if ChunkName = 'bKGD' then
        begin
          if Assigned(FBackgroundChunk) then
            raise EPngError.Create(RCStrSeveralBackgroundChunks);
          FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
          FBackgroundChunk.ReadFromStream(MemoryStream);
          AddChunk(FBackgroundChunk);
        end
        else
        begin
          ChunkClass := TCustomDefinedChunkWithHeaderClass(ChunkClassByChunkName(ChunkName));
          if ChunkClass <> nil then
          begin
            Chunk := ChunkClass.Create(FImageHeader);
            Chunk.ChunkFlags := [cfSizeFirst];
            Chunk.ReadFromStream(MemoryStream);
            FAdditionalChunkList.Add(Chunk);
            AddChunk(Chunk);
          end
          else
          begin
            // check if chunk is ancillary
            if (Byte(ChunkName[0]) and $80) = 0 then
              ReadUnknownChunk(MemoryStream, ChunkName, ChunkSize)
            else
              raise EPngError.Create(RCStrAncillaryUnknownChunk);
          end;
        end;

        // read & check CRC
        Read(ChunkCRC, 4);
        {$IFDEF CheckCRC}
        if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
          raise EPngError.Create(RCStrCRCError);
        {$ENDIF}
      end;
    finally
      FreeAndNil(MemoryStream);
    end;
  end;
end;

procedure TFilePng.SaveToStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  CRC          : Cardinal;
  MemoryStream : TMemoryStream;
  Index        : Integer;

  procedure SaveChunkToStream(Chunk: TCustomChunk);
  begin
    MemoryStream.Clear;

    // save chunk to memory stream
    Chunk.SaveToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(MemoryStream, MemoryStream.Size);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Stream.Write(CRC, SizeOf(Cardinal));
  end;

begin
  with Stream do
  begin
    // write chunk ID
    ChunkName := GetClassChunkName;
    Write(ChunkName, 4);

    // write PNG magic
    ChunkName := CPngMagic;
    Write(ChunkName, 4);

    MemoryStream := TMemoryStream.Create;
    try
      // store chunk size directly to stream
      ChunkSize := FImageHeader.ChunkSize;
      WriteSwappedCardinal(Stream, ChunkSize);

      // store chunk name to memory stream
      ChunkName := FImageHeader.ChunkName;
      MemoryStream.Write(ChunkName, 4);

      // save image header to memory stream
      FImageHeader.WriteToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));

      // eventually save physical pixel dimensions chunk
      if Assigned(FPhysicalDimensions) then
        SaveChunkToStream(FPhysicalDimensions);

      // eventually save significant bits chunk
      if Assigned(FSignificantBits) then
        SaveChunkToStream(FSignificantBits);

      // eventually save gamma chunk
      if Assigned(FGammaChunk) then
        SaveChunkToStream(FGammaChunk);

      // eventually save chroma chunk
      if Assigned(FChromaChunk) then
        SaveChunkToStream(FChromaChunk);

      // eventually save palette chunk
      if Assigned(FPaletteChunk) then
        SaveChunkToStream(FPaletteChunk);

      // eventually save transparency chunk
      if Assigned(FTransparencyChunk) then
        SaveChunkToStream(FTransparencyChunk);

      // eventually save background chunk
      if Assigned(FBackgroundChunk) then
        SaveChunkToStream(FBackgroundChunk);

      // store additional chunks
      for Index := 0 to FAdditionalChunkList.Count - 1 do
        SaveChunkToStream(TCustomChunk(FAdditionalChunkList[Index]));

      // save data streams
      for Index := 0 to FDataChunkList.Count - 1 do
        SaveChunkToStream(TCustomChunk(FDataChunkList[Index]));
    finally
      FreeAndNil(MemoryStream);
    end;

    // write chunk size
    WriteSwappedCardinal(Stream, 0);

    // write chunk ID
    ChunkName := 'IEND';
    Write(ChunkName, 4);

    // write CRC
    CRC := 2187346606;
    Write(CRC, 4);
  end;
end;

procedure TFilePng.ReadUnknownChunk(Stream: TStream;
  ChunkName: TChunkName; ChunkSize: Integer);
var
  UnknownChunk : TChunkPngUnknown;
begin
  UnknownChunk := TChunkPngUnknown.Create(ChunkName);
  UnknownChunk.LoadFromStream(Stream);
  FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TFilePng.ReadImageDataChunk(Stream: TStream; Size: Integer);
var
  ImageDataChunk : TChunkPngImageData;
begin
  ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
  ImageDataChunk.ReadFromStream(Stream);
  FDataChunkList.Add(ImageDataChunk);
  AddChunk(ImageDataChunk);
end;

procedure TFilePng.Assign(Source: TPersistent);
begin
  if Source is TFilePng then
    with TFilePng(Source) do
    begin
      if Assigned(Self.FImageHeader) then
        Self.FImageHeader.Assign(FImageHeader);

      // assign palette chunk
      if Assigned(Self.FPaletteChunk) then
        if Assigned(FPaletteChunk) then
          Self.FPaletteChunk.Assign(FPaletteChunk)
        else
          FreeAndNil(Self.FPaletteChunk)
      else if Assigned(FPaletteChunk) then
      begin
        Self.FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        Self.FPaletteChunk.Assign(FPaletteChunk);
      end;

      // assign gamma chunk
      if Assigned(Self.FGammaChunk) then
        if Assigned(FGammaChunk) then
          Self.FGammaChunk.Assign(FGammaChunk)
        else
          FreeAndNil(Self.FGammaChunk)
      else if Assigned(FGammaChunk) then
      begin
        Self.FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        Self.FGammaChunk.Assign(FGammaChunk);
      end;

      // assign time chunk
      if Assigned(Self.FTimeChunk) then
        if Assigned(FTimeChunk) then
          Self.FTimeChunk.Assign(FTimeChunk)
        else
          FreeAndNil(Self.FTimeChunk)
      else if Assigned(FTimeChunk) then
      begin
        Self.FTimeChunk := TChunkPngTime.Create(FImageHeader);
        Self.FTimeChunk.Assign(FTimeChunk);
      end;

      // assign significant bits
      if Assigned(Self.FSignificantBits) then
        if Assigned(FSignificantBits) then
          Self.FSignificantBits.Assign(FSignificantBits)
        else
          FreeAndNil(Self.FSignificantBits)
      else if Assigned(FSignificantBits) then
      begin
        Self.FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
        Self.FSignificantBits.Assign(FSignificantBits);
      end;

      // assign physical dimensions
      if Assigned(Self.FPhysicalDimensions) then
        if Assigned(FPhysicalDimensions) then
          Self.FPhysicalDimensions.Assign(FPhysicalDimensions)
        else
          FreeAndNil(Self.FPhysicalDimensions)
      else if Assigned(FPhysicalDimensions) then
      begin
        Self.FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
        Self.FPhysicalDimensions.Assign(FPhysicalDimensions);
      end;

      // assign primary chromaticities
      if Assigned(Self.FChromaChunk) then
        if Assigned(FChromaChunk) then
          Self.FChromaChunk.Assign(FChromaChunk)
        else
          FreeAndNil(Self.FChromaChunk)
      else if Assigned(FChromaChunk) then
      begin
        Self.FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
        Self.FChromaChunk.Assign(FChromaChunk);
      end;

      // assign transparency
      if Assigned(Self.FTransparencyChunk) then
        if Assigned(FTransparencyChunk) then
          Self.FTransparencyChunk.Assign(FTransparencyChunk)
        else
          FreeAndNil(Self.FTransparencyChunk)
      else if Assigned(FTransparencyChunk) then
      begin
        Self.FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
        Self.FTransparencyChunk.Assign(FTransparencyChunk);
      end;

      // assign background
      if Assigned(Self.FBackgroundChunk) then
        if Assigned(FBackgroundChunk) then
          Self.FBackgroundChunk.Assign(FBackgroundChunk)
        else
          FreeAndNil(Self.FBackgroundChunk)
      else if Assigned(FBackgroundChunk) then
      begin
        Self.FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
        Self.FBackgroundChunk.Assign(FBackgroundChunk);
      end;

      if Assigned(Self.FDataChunkList) then
        Self.FDataChunkList.Assign(FDataChunkList);
      if Assigned(Self.FAdditionalChunkList) then
        Self.FAdditionalChunkList.Assign(FAdditionalChunkList);
     end
  else
    inherited;
end;

procedure TFilePng.AssignTo(Dest: TPersistent);
begin
  if Dest is TFilePng then
    with TFilePng(Dest) do
    begin
      FImageHeader.Assign(Self.FImageHeader);
      FPaletteChunk.Assign(Self.FPaletteChunk);
      FGammaChunk.Assign(Self.FGammaChunk);
      FTimeChunk.Assign(Self.FTimeChunk);
      FSignificantBits.Assign(Self.FSignificantBits);
      FPhysicalDimensions.Assign(Self.FPhysicalDimensions);
      FChromaChunk.Assign(Self.FChromaChunk);
      FTransparencyChunk.Assign(Self.FTransparencyChunk);
      FBackgroundChunk.Assign(Self.FBackgroundChunk);
      FDataChunkList.Assign(Self.FDataChunkList);
      FAdditionalChunkList.Assign(Self.FAdditionalChunkList);
    end
  else
    inherited;
end;

function TFilePng.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue : Cardinal;
  Value    : Byte;
begin
  if Stream is TMemoryStream then
    Result := CalculateCRC(TMemoryStream(Stream).Memory, Stream.Size)
  else
    with Stream do
    begin
      Seek(0, soFromBeginning);

      // initialize CRC
      CrcValue := $FFFFFFFF;
      {$IFDEF FPC}
      Value := 0;
      {$ENDIF}

      while Position < Size do
      begin
        Read(Value, 1);

        CrcValue := GCrcTable^[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
      end;

      Result := (CrcValue xor $FFFFFFFF);

      Seek(0, soFromBeginning);
    end;
end;

function TFilePng.CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
var
  CrcValue : Cardinal;
  Pos      : Cardinal;
begin
  // ignore size (offset by 4 bytes)
  Pos := 0;

  // initialize CRC
  CrcValue := $FFFFFFFF;

  while Pos < Count do
  begin
    CrcValue := GCrcTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
    Inc(Buffer);
    Inc(Pos);
  end;

  Result := (CrcValue xor $FFFFFFFF);
{$ELSE}
asm
{$IFDEF Target_x64}
        PUSH    RBX
        PUSH    RDI
        MOV     RCX, R8
        JS      @Done
        NEG     RCX
        MOV     RBX, $FFFFFFFF

{$IFNDEF FPC}
        MOV     RDI, [GCrcTable]
{$ELSE}
        MOV     RDI, [RIP + GCrcTable]
{$ENDIF}

@Start:
        MOV     EAX, [RDX]
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [RDI + 4 * RAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     RDX
        INC     RCX
        JS      @Start

        XOR     EBX, $FFFFFFFF
        MOV     RAX, RBX

@Done:
        POP     RDI
        POP     RBX
{$ELSE}
        PUSH    EBX
        PUSH    EDI
        JS      @Done
        NEG     ECX
        MOV     EBX, $FFFFFFFF

{$IFNDEF FPC}
        MOV     EDI, [GCrcTable]
{$ELSE}
        MOV     EDI, [EIP + GCrcTable]
{$ENDIF}

@Start:
        MOVZX   EAX, [EDX]
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [EDI + 4 * EAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     EDX
        INC     ECX
        JS      @Start

        XOR     EBX, $FFFFFFFF
        MOV     Result, EBX

@Done:
        POP     EDI
        POP     EBX
{$ENDIF}
{$ENDIF}
end;

{$IFDEF CheckCRC}
function TFilePng.CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
begin
  Result := CalculateCRC(Stream) = CRC;
end;
{$ENDIF}

function TFilePng.GetBitDepth: Byte;
begin
  Result := FImageHeader.BitDepth;
end;

class function TFilePng.GetClassChunkName: TChunkName;
begin
  Result := '‰PNG';
end;

function TFilePng.GetColorType: TColorType;
begin
  Result := FImageHeader.ColorType;
end;

function TFilePng.GetCompressionMethod: Byte;
begin
  Result := FImageHeader.CompressionMethod;
end;

function TFilePng.GetFilterMethod: TFilterMethod;
begin
  Result := FImageHeader.FilterMethod;
end;

function TFilePng.GetFilterMethods: TAvailableAdaptiveFilterMethods;
begin
  Result := FImageHeader.FAdaptiveFilterMethods;
end;

function TFilePng.GetGamma: Single;
begin
  if Assigned(FGammaChunk) then
    Result := FGammaChunk.GammaAsSingle
  else
    Result := 1;
end;

function TFilePng.GetHeight: Integer;
begin
  Result := FImageHeader.Height;
end;

function TFilePng.GetInterlaceMethod: TInterlaceMethod;
begin
  Result := FImageHeader.InterlaceMethod;
end;

function TFilePng.GetModifiedTime: TDateTime;
begin
  if Assigned(FTimeChunk) then
    with FTimeChunk do
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0)
  else
    Result := 0;
end;

function TFilePng.GetPaletteEntry(Index: Integer): TRGB24;
begin
  if Assigned(FPaletteChunk) then
    Result := FPaletteChunk.PaletteEntry[Index]
  else
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TFilePng.GetPaletteEntryCount: Integer;
begin
  if Assigned(FPaletteChunk) then
    Result := FPaletteChunk.Count
  else
    Result := 0;
end;

function TFilePng.GetPixelsPerUnitX: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelsPerUnitX
  else
    Result := 1;
end;

function TFilePng.GetPixelsPerUnitY: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelsPerUnitY
  else
    Result := 1;
end;

function TFilePng.GetPixelUnit: Byte;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelUnit
  else
    Result := 0;
end;

function TFilePng.GetWidth: Integer;
begin
  Result := FImageHeader.Width;
end;

function TFilePng.HasGammaInformation: Boolean;
begin
  Result := Assigned(FGammaChunk);
end;

function TFilePng.HasModifiedTimeInformation: Boolean;
begin
  Result := Assigned(FTimeChunk);
end;

function TFilePng.HasPhysicalPixelDimensionsInformation: Boolean;
begin
  Result := Assigned(FPhysicalDimensions);
end;

procedure TFilePng.Clear;
begin
  // clear chunk lists
  FDataChunkList.Clear;
  FAdditionalChunkList.Clear;

  // reset image header to default
  FImageHeader.ResetToDefault;

  // free palette chunk
  if Assigned(FPaletteChunk) then
    FreeAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
    FreeAndNil(FGammaChunk);

  // free gamma chunk
  if Assigned(FChromaChunk) then
    FreeAndNil(FChromaChunk);

  // free transparency chunk
  if Assigned(FTransparencyChunk) then
    FreeAndNil(FTransparencyChunk);

  // free background chunk
  if Assigned(FBackgroundChunk) then
    FreeAndNil(FBackgroundChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
    FreeAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
    FreeAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
    FreeAndNil(FPhysicalDimensions);
end;


procedure BuildCrcTable(Polynomial: Cardinal);
var
  c    : Cardinal;
  n, k : Integer;
begin
  // allocate CRC table memory
  GetMem(GCrcTable, 256 * SizeOf(Cardinal));

  // fill CRC table
  for n := 0 to 255 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := Polynomial xor (c shr 1)
      else
        c := c shr 1;
    end;
    GCrcTable^[n] := c;
  end;
end;

initialization
  BuildCrcTable($EDB88320);
  TFilePng.RegisterChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
    TChunkPngStandardColorSpaceRGB, TChunkPngPrimaryChromaticities,
    TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
    TChunkPngPhysicalPixelDimensions, TChunkPngText, //TChunkPngSuggestedPalette,
    TChunkPngCompressedText, TChunkPngInternationalText,
    TChunkPngImageHistogram, TChunkPngBackgroundColor,
    TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);

finalization
  if Assigned(GCrcTable) then Dispose(GCrcTable);

end.
