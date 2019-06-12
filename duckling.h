#include <stdbool.h>


//// Initializion/Shutdown

bool ducklingInit();
void ducklingExit();

void ducklingFreeString(char *string);

void ducklingFreeObject(void *object);



//// Dimension

typedef void *DucklingDimensionPtr;

DucklingDimensionPtr ducklingParseDimension(char *dimension);
char *ducklingGetDimensionName(DucklingDimensionPtr dimension);
DucklingDimensionPtr ducklingGetBuiltinDimensionCreditCardNumber();
DucklingDimensionPtr ducklingGetBuiltinDimensionDistance();
DucklingDimensionPtr ducklingGetBuiltinDimensionDuration();
DucklingDimensionPtr ducklingGetBuiltinDimensionEmail();
DucklingDimensionPtr ducklingGetBuiltinDimensionAmountOfMoney();
DucklingDimensionPtr ducklingGetBuiltinDimensionNumeral();
DucklingDimensionPtr ducklingGetBuiltinDimensionOrdinal();
DucklingDimensionPtr ducklingGetBuiltinDimensionPhoneNumber();
DucklingDimensionPtr ducklingGetBuiltinDimensionQuantity();
DucklingDimensionPtr ducklingGetBuiltinDimensionTemperature();
DucklingDimensionPtr ducklingGetBuiltinDimensionTime();
DucklingDimensionPtr ducklingGetBuiltinDimensionUrl();
DucklingDimensionPtr ducklingGetBuiltinDimensionVolume();


//// Lang

typedef void *DucklingLangPtr;

DucklingLangPtr ducklingParseLang(char *lang);


//// Region

typedef void *DucklingRegionPtr;

DucklingRegionPtr ducklingParseRegion(char *region);


//// Locale

typedef void *DucklingLocalePtr;

DucklingLocalePtr ducklingMakeLocale(DucklingLangPtr lang, DucklingRegionPtr region);


//// TimeZoneSeriesMap

typedef void *DucklingTimeZoneSeriesMapPtr;

DucklingTimeZoneSeriesMapPtr ducklingLoadTimeZoneSeriesMap(char *base);


//// TimeZoneSeries

typedef void *DucklingTimeZoneSeriesPtr;

DucklingTimeZoneSeriesPtr ducklingGetTimeZoneSeries(DucklingTimeZoneSeriesMapPtr timeZoneSeriesMap, char *name);


//// Time

typedef void *DucklingTimePtr;

DucklingTimePtr ducklingGetCurrentTime(DucklingTimeZoneSeriesPtr timeZoneSeries);
DucklingTimePtr ducklingMakeTime(double timestamp, DucklingTimeZoneSeriesPtr timeZoneSeries);


//// Parse

char *ducklingParseText(char *text, DucklingTimePtr time, DucklingLocalePtr locale, bool latent, DucklingDimensionPtr dimensions[]);
