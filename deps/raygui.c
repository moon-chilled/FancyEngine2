#define RAYGUI_IMPLEMENTATION
#define RAYGUI_STANDALONE

// needed when RAYGUI_STANDALONE so raygui doesn't define its own bool
#include <stdbool.h>

//struct Texture2D { void *texture; };
//struct Font { void *font; };

#include "raygui/src/raygui.h"

int raygui_KEY_RIGHT = KEY_RIGHT;
int raygui_KEY_LEFT = KEY_LEFT;
int raygui_KEY_DOWN = KEY_DOWN;
int raygui_KEY_UP = KEY_UP;
int raygui_KEY_BACKSPACE = KEY_BACKSPACE;
int raygui_KEY_ENTER = KEY_ENTER;
int raygui_MOUSE_LEFT_BUTTON = MOUSE_LEFT_BUTTON;


Vector2 (*real_GetMousePosition)(void);
static Vector2 GetMousePosition(void) { return real_GetMousePosition(); }

int (*real_GetMouseWheelMove)(void);
static int GetMouseWheelMove(void) { return real_GetMouseWheelMove(); }

bool (*real_IsMouseButtonDown)(int button);
static bool IsMouseButtonDown(int button) { return real_IsMouseButtonDown(button); }

bool (*real_IsMouseButtonPressed)(int button);
static bool IsMouseButtonPressed(int button) { return real_IsMouseButtonPressed(button); }

bool (*real_IsMouseButtonReleased)(int button);
static bool IsMouseButtonReleased(int button) { return real_IsMouseButtonReleased(button); }

bool (*real_IsKeyDown)(int key);
static bool IsKeyDown(int key) { return real_IsKeyDown(key); }

bool (*real_IsKeyPressed)(int key);
static bool IsKeyPressed(int key) { return real_IsKeyPressed(key); }

int (*real_GetKeyPressed)(void);
static int GetKeyPressed(void) { return real_GetKeyPressed(); }

void (*real_DrawRectangle)(int x, int y, int width, int height, Color color);
static void DrawRectangle(int x, int y, int width, int height, Color color) { return real_DrawRectangle(x, y, width, height, color); }

void (*real_DrawRectangleGradientEx)(Rectangle rec, Color col1, Color col2, Color col3, Color col4);
static void DrawRectangleGradientEx(Rectangle rec, Color col1, Color col2, Color col3, Color col4) { return real_DrawRectangleGradientEx(rec, col1, col2, col3, col4); }

void (*real_DrawTriangle)(Vector2 v1, Vector2 v2, Vector2 v3, Color color);
static void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3, Color color) { return real_DrawTriangle(v1, v2, v3, color); }

void (*real_DrawTextureRec)(Texture2D texture, Rectangle sourceRec, Vector2 position, Color tint);
static void DrawTextureRec(Texture2D texture, Rectangle sourceRec, Vector2 position, Color tint) { return real_DrawTextureRec(texture, sourceRec, position, tint); }

Font (*real_GetFontDefault)(void);
static Font GetFontDefault(void) { return real_GetFontDefault(); }

Vector2 (*real_MeasureTextEx)(Font font, const char *text, float fontSize, float spacing);
static Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing) { return real_MeasureTextEx(font, text, fontSize, spacing); }

void (*real_DrawTextEx)(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint);
static void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint) { return real_DrawTextEx(font, text, position, fontSize, spacing, tint); }

Font (*real_LoadFontEx)(const char *fileName, int fontSize, int *fontChars, int charsCount);
static Font LoadFontEx(const char *fileName, int fontSize, int *fontChars, int charsCount) { return real_LoadFontEx(fileName, fontSize, fontChars, charsCount); }
