import appModuleHandler
import api
import ui
import os
import struct
import time
from NVDAObjects import behaviors
from textInfos.offsets import OffsetsTextInfo
import socket
import tones

_client: socket.socket | None = None


def _initClient(retries=20, delay=0.05) -> bool:
    global _client
    import tones

    tones.beep(1500, 50)
    home = os.environ.get("HOME") or os.environ.get("USERPROFILE")
    if not home:
        raise RuntimeError(
            "Cannot determine home directory – both HOME and USERPROFILE are missing"
        )

    port_path = os.path.join(home, ".emacs.d", ".eval-server-port")

    for attempt in range(retries):
        try:
            if not os.path.isfile(port_path):
                time.sleep(delay)
                continue

            with open(port_path, "r", encoding="utf-8") as f:
                port_str = f.read().strip()
            port = int(port_str)

            _client = socket.create_connection(("127.0.0.1", port), timeout=10)
            return True
        except (ConnectionRefusedError, FileNotFoundError, ValueError, socket.timeout):
            time.sleep(delay)
            continue
        except Exception as e:
            print(f"Unexpected error in _initClient: {e}")
            time.sleep(delay)

    _client = None
    return False


def _sendAll(data: bytes):
    global _client
    if _client is None:
        raise RuntimeError(
            "Client is not initialized. Call _initClient first.")
    try:
        _client.sendall(data)
    except (BrokenPipeError, ConnectionResetError):
        print("Connection failed during sendall — attempting to restart connection...")
        if _initClient():
            _client.sendall(data)
        else:
            raise RuntimeError(
                "Failed to re-establish connection during sendall")


def _recvAll(n: int) -> bytes:
    global _client
    if _client is None:
        raise RuntimeError(
            "Client is not initialized. Call _initClient first.")
    data = b""
    while len(data) < n:
        try:
            packet = _client.recv(n - len(data))
            if not packet:
                raise ConnectionError("Connection closed by the server")
            data += packet
        except (BrokenPipeError, ConnectionResetError):
            print("Connection failed during recv — attempting to restart connection...")
            if _initClient():
                return _recvAll(n)  # Retry from scratch
            else:
                raise RuntimeError(
                    "Failed to re-establish connection during recv")
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


class MinibufferTextInfo(OffsetsTextInfo):
    def _getStoryText(self):
        # Get the prompt and contents as plain text, stripping any text properties
        return _emacsEval("(nvda-minibuffer-get-story-text)")

    def _getStoryLength(self):
        return len(self._getStoryText())

    def _getTextRange(self, start, end):
        text = self._getStoryText()
        return text[start:end]

    def _getCaretOffset(self):
        return _emacsInt("(nvda-minibuffer-get-caret-offset)")

    def _setCaretOffset(self, offset):
        _emacsEval(f"(nvda-minibuffer-set-caret-offset {offset})")


class EmacsTextInfo(OffsetsTextInfo):
    def _getStoryLength(self):
        return _emacsInt("(nvda-get-story-length)")

    def _getCaretOffset(self):
        return _emacsInt("(nvda-get-caret-offset)")

    def _setCaretOffset(self, offset):
        _emacsEval(f"(nvda-set-caret-offset {offset})")

    def _getSelectionOffsets(self):
        result = _emacsEval("(nvda-get-selection-offsets)")
        start, end = map(int, result.split(','))
        return (start, end) if start <= end else (end, start)

    def _getLineNumFromOffset(self, offset):
        return _emacsInt(f"(nvda-get-line-num-from-offset {offset})")

    def _getLineOffsets(self, offset):
        result = _emacsEval(f"(nvda-get-line-offsets {offset})")
        start, end = map(int, result.split(','))
        return [start, end]

    def _getTextRange(self, start, end):
        # nvda-get-text-range handles all validation and clamping
        result = _emacsEval(f"(nvda-get-text-range {start} {end})")
        return result if result else ""


class Emacs(behaviors.EditableTextWithoutAutoSelectDetection):
    def _get_TextInfo(self):
        # Check if we are in the minibuffer
        if _emacsInt("(nvda-in-minibuffer-p)") == 1:
            return MinibufferTextInfo
        else:
            return EmacsTextInfo

    def script_sayLineOffsets(self, gesture):
        ti = self.makeTextInfo("caret")
        startOffset, endOffset = ti._getLineOffsets(ti._startOffset)
        ui.message(f"{ti._startOffset}, {startOffset}, {endOffset}")

    def script_sayVisibility(self, gesture):
        vis = _emacsEval("(nvda-point-invisible-p)")
        ui.message(f"{vis}")

    __gestures = {
        "kb:NVDA+L": "sayLineOffsets",
        "kb:NVDA+I": "sayVisibility",
    }


class AppModule(appModuleHandler.AppModule):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if not _initClient():
            ui.browseableMessage("Eval server is probably not running.")

    def terminate(self):
        global _client
        super().terminate()
        _client = None
        tones.beep(500, 50)

    def chooseNVDAObjectOverlayClasses(self, obj, clsList):
        if obj.windowClassName == "Emacs":
            clsList.insert(0, Emacs)
