#include <windows.h>
#include <stdio.h>
#include <io.h>

extern int redirectin;

static int once = 1;

static DWORD savemode;  // To save the current console mode
static BOOL savebrk;    // To save the current Ctrl-Break state

/* setraw -- set raw mode for console input */
void setraw(void)
{
    HANDLE hConsole;
    DWORD mode;

    // Get the handle to the standard input (console)
    hConsole = GetStdHandle(STD_INPUT_HANDLE);

    // Get the current console mode and save it
    if (!GetConsoleMode(hConsole, &savemode)) {
        fprintf(stderr, "Error getting console mode\n");
        return;
    }

    // Modify the console mode to disable line input and echo input (raw mode)
    mode = savemode;
    mode &= ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
    mode |= ENABLE_PROCESSED_INPUT;

    // Set the console to raw mode
    if (!SetConsoleMode(hConsole, mode)) {
        fprintf(stderr, "Error setting console mode\n");
        return;
    }

    // Disable Ctrl-Break handling by installing a custom handler
    if (!SetConsoleCtrlHandler(NULL, TRUE)) {
        fprintf(stderr, "Error disabling Ctrl-Break\n");
    }
}

/* resetconsole -- reset console mode to the saved state */
static void resetconsole(void)
{
    HANDLE hConsole = GetStdHandle(STD_INPUT_HANDLE);

    // Restore the saved console mode
    SetConsoleMode(hConsole, savemode);

    // Restore the original Ctrl-Break behavior
    SetConsoleCtrlHandler(NULL, FALSE);
}

#ifdef GRAPHICS
static int origmode;    // Original graphics mode (if GRAPHICS is defined)
#endif

/* unsetraw -- restore original console mode */
void unsetraw(void)
{
    HANDLE hConsole;

    // Get the handle to the standard input (console)
    hConsole = GetStdHandle(STD_INPUT_HANDLE);

    // Restore the saved console mode
    if (!SetConsoleMode(hConsole, savemode)) {
        fprintf(stderr, "Error restoring console mode\n");
    }

    // Restore the Ctrl-Break handler
    if (!SetConsoleCtrlHandler(NULL, FALSE)) {
        fprintf(stderr, "Error restoring Ctrl-Break handler\n");
    }

#ifdef GRAPHICS
    // Restore original graphics mode if GRAPHICS is defined
    // You would need to define `setgmode()` and `ourmode1`, `ourmode2`, `origmode`
    if (ourmode1 != 0 && ourmode2 != origmode) {
        setgmode(origmode, 0);  // Restore to original mode
    }
#endif
}

int xgetc() {
    int ch;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();
#endif
    fflush(stderr);  // flushbuf()

    if (once) {
	once = 0;
	if (!_isatty(_fileno(stdin)))
		redirectin = 1;
    }
    if (!redirectin) {
        // Use standard input, replace bdos(7, 0, 0) with getchar()
        ch = getchar();  // Get character from the console
        if (ch == 0 || ch == 0xE0) {  // Handle extended keys (e.g., function keys)
            ch = 256 + getchar();  // Combine with the next character
        }
    } else {
        // Handle redirected input using the Windows API or standard read
        unsigned char temp[1];
        DWORD bytesRead;
        
        // Read from the file handle (2 is stderr in this case)
        if (!ReadFile(GetStdHandle(STD_INPUT_HANDLE), temp, 1, &bytesRead, NULL) || bytesRead == 0) {
            ch = EOF;  // Handle end-of-file or error
        } else {
            ch = temp[0];  // Read the character from redirected input
        }
    }

#ifdef TIMES
    kbtime += real_tick_count() - kbtime;
#endif
    return ch;
}

