import appModuleHandler
from scriptHandler import script
import ui
import api
import os
import socket
import struct
import time
from NVDAObjects import behaviors
from NVDAObjects.window import Window
import textInfos

_client = None


def _initClient(retries=20, delay=0.05) -> bool:
    global _client
    import tones

    tones.beep(1500, 50)
    home = os.environ.get("HOME") or os.environ.get("USERPROFILE")
    if not home:
        raise RuntimeError("Cannot determine home directory – both HOME and USERPROFILE are missing")

    port_path = os.path.join(home, ".emacs.d", ".eval-server-port")

    for attempt in range(retries):
        try:
            if not os.path.isfile(port_path):
                time.sleep(delay)
                continue

            with open(port_path, "r", encoding="utf-8") as f:
                port_str = f.read().strip()
            port = int(port_str)

            _client = socket.create_connection(("127.0.0.1", port), timeout=1)
            return True
        except (ConnectionRefusedError, FileNotFoundError, ValueError, socket.timeout):
            time.sleep(delay)
            continue
        except Exception as e:
            print(f"Neočakávaná chyba pri _initClient: {e}")
            time.sleep(delay)

    _client = None
    return False


def _terminateClient():
    global _client
    if _client:
        try:
            _client.close()
        except Exception:
            pass
        _client = None


def _sendAll(data: bytes):
    global _client
    try:
        _client.sendall(data)
    except (BrokenPipeError, ConnectionResetError):
        print("Spojenie zlyhalo pri sendall — pokúšam sa reštartovať spojenie...")
        if _initClient():
            _client.sendall(data)
        else:
            raise RuntimeError("Nepodarilo sa znovu vytvoriť spojenie pri sendall")


def _recvAll(n: int) -> bytes:
    global _client
    data = b""
    while len(data) < n:
        try:
            packet = _client.recv(n - len(data))
            if not packet:
                raise ConnectionError("Spojenie s Emacs serverom sa prerušilo.")
            data += packet
        except (BrokenPipeError, ConnectionResetError):
            print("Spojenie zlyhalo pri recv — pokúšam sa reštartovať spojenie...")
            if _initClient():
                return _recvAll(n)  # Retry from scratch
            else:
                raise RuntimeError("Nepodarilo sa znovu vytvoriť spojenie pri recv")
    return data


def _emacsEval(expr: str) -> str:
    global _client
    if _client is None:
        return ""

    data = expr.encode("utf-8")
    _sendAll(struct.pack(">I", len(data)) + data)

    raw_len = _recvAll(4)
    (resp_len,) = struct.unpack(">I", raw_len)
    resp = _recvAll(resp_len)

    result = resp.decode("utf-8")
    if result.startswith('"') and result.endswith('"'):
        result = result[1:-1]
    return result


def _emacsInt(expr: str) -> int:
    try:
        res = _emacsEval(expr)
        return int(res)
    except ValueError:
        return 0


class MinibufferTextInfo(textInfos.offsets.OffsetsTextInfo):
    def _getStoryText(self):
        return _emacsEval("(minibuffer-prompt)") + _emacsEval("(minibuffer-contents)")

    def _getStoryLength(self):
        return len(self._getStoryText())

    def _getTextRange(self, start, end):
        text = self._getStoryText()
        return text[start:end]

    def _getCaretOffset(self):
        return _emacsInt("(1- (point))")

    def _setCaretOffset(self, offset):
        _emacsEval(f"(goto-char {offset + 1})")


class EmacsTextInfo(textInfos.offsets.OffsetsTextInfo):
    def _getStoryLength(self):
        return _emacsInt("(- (point-max) (point-min))")

    def _getCaretOffset(self):
        return _emacsInt("(point)") - 1

    def _setCaretOffset(self, offset):
        _emacsEval(f"(goto-char {offset + 1})")

    def _getSelectionOffsets(self):
        if _emacsEval("(use-region-p)") == "t":
            start = _emacsInt("(1- (region-beginning))")
            end = _emacsInt("(1- (region-end))")
            return (start, end) if start <= end else (end, start)
        else:
            caret = self._getCaretOffset()
            return (caret, caret)

    def _getLineNumFromOffset(self, offset):
        return _emacsInt(f"(line-number-at-pos {offset + 1} t)")

    def _getLineOffsets(self, offset):
        return [
            _emacsInt(
                f"(save-excursion (goto-char {offset + 1}) (beginning-of-visual-line) (1- (point)))"
            ),
            _emacsInt(
                f"(save-excursion (goto-char {offset + 1}) (end-of-visual-line) (point))"
            ),
        ]

    def _getTextRange(self, start, end):
        if start >= end:
            return ""
        pointMax = _emacsInt("(point-max)")
        start += 1
        end += 1
        if end >= pointMax:
            end = pointMax
        return _emacsEval(f"(buffer-substring-no-properties {start} {end})")


class Emacs(behaviors.EditableTextWithoutAutoSelectDetection):
    def _get_TextInfo(self):
        # Skontroluj, či sme v minibuffri
        if _emacsEval("(minibufferp)") == "t":
            return MinibufferTextInfo
        else:
            return EmacsTextInfo


class AppModule(appModuleHandler.AppModule):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if not _initClient():
            ui.browseableMessage("Eval server is probably not running.")

    def terminate(self):
        global _client
        super().terminate()
        _client = None
        import tones

        tones.beep(500, 50)

    def chooseNVDAObjectOverlayClasses(self, obj, clsList):
        if obj.windowClassName == "Emacs":
            clsList.insert(0, Emacs)

    @script(gesture="kb:nvda+shift+v")
    def script_pokus(self, gesture):
        ui.browseableMessage(self.appPath)

    @script(
        description=_("Announces the window class name of the current focus object"),
        gesture="kb:NVDA+leftArrow",
    )
    def script_announceWindowClassName(self, gesture):
        focusObj = api.getFocusObject()
        name = focusObj.name
        windowClassName = focusObj.windowClassName
        ui.message(f"class for {name} window: {windowClassName}")

    @script(
        description=_("Announces the window control ID of the current focus object"),
        gesture="kb:NVDA+rightArrow",
    )
    def script_announceWindowControlID(self, gesture):
        focusObj = api.getFocusObject()
        name = focusObj.name
        windowControlID = focusObj.windowControlID
        ui.message(f"Control ID for {name} window: {windowControlID}")
