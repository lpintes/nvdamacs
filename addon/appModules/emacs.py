import appModuleHandler
import api
import ui
import os
from NVDAObjects import behaviors
from textInfos.offsets import OffsetsTextInfo
import tones
from . import rpc

rpcClient: rpc.RpcClient | None = None


def initClient(retries=20, delay=0.05) -> bool:
    """Initialize RPC client connection to Emacs server."""
    global rpcClient
    import time

    tones.beep(1500, 50)
    home = os.environ.get("HOME") or os.environ.get("USERPROFILE")
    if not home:
        raise RuntimeError(
            "Cannot determine home directory – both HOME and USERPROFILE are missing"
        )

    portFile = os.path.join(home, ".emacs.d", ".eval-server-port")

    # Retry connection
    for attempt in range(retries):
        try:
            if not os.path.isfile(portFile):
                time.sleep(delay)
                continue

            rpcClient = rpc.RpcClient(portFile)
            if rpcClient.connect():
                return True

            time.sleep(delay)
        except Exception as e:
            print(f"Unexpected error in initClient: {e}")
            time.sleep(delay)

    rpcClient = None
    return False


class MinibufferTextInfo(OffsetsTextInfo):
    def _getStoryText(self):
        # Get the prompt and contents as plain text, stripping any text properties
        return rpcClient.request("minibufferGetStoryText")

    def _getStoryLength(self):
        return len(self._getStoryText())

    def _getTextRange(self, start, end):
        text = self._getStoryText()
        return text[start:end]

    def _getCaretOffset(self):
        return rpcClient.request("minibufferGetCaretOffset")

    def _setCaretOffset(self, offset):
        rpcClient.request("minibufferSetCaretOffset", {"offset": offset})


class EmacsTextInfo(OffsetsTextInfo):
    def _getStoryLength(self):
        return rpcClient.request("getStoryLength")

    def _getCaretOffset(self):
        return rpcClient.request("getCaretOffset")

    def _setCaretOffset(self, offset):
        rpcClient.request("setCaretOffset", {"offset": offset})

    def _getSelectionOffsets(self):
        result = rpcClient.request("getSelectionOffsets")
        start = result["start"]
        end = result["end"]
        return (start, end) if start <= end else (end, start)

    def _getLineNumFromOffset(self, offset):
        return rpcClient.request("getLineNumFromOffset", {"offset": offset})

    def _getLineOffsets(self, offset):
        result = rpcClient.request("getLineOffsets", {"offset": offset})
        return [result["startOffset"], result["endOffset"]]

    def _getCharacterOffsets(self, offset):
        result = rpcClient.request("getCharacterOffsets", {"offset": offset})
        return [result["startOffset"], result["endOffset"]]

    def _getWordOffsets(self, offset):
        result = rpcClient.request("getWordOffsets", {"offset": offset})
        return [result["startOffset"], result["endOffset"]]

    def _getTextRange(self, start, end):
        # nvda-get-text-range handles all validation and clamping
        result = rpcClient.request("getTextRange", {"start": start, "end": end})
        return result if result else ""


class Emacs(behaviors.EditableTextWithoutAutoSelectDetection):
    def _get_TextInfo(self):
        # Check if we are in the minibuffer
        if rpcClient.request("inMinibufferP") == 1:
            return MinibufferTextInfo
        else:
            return EmacsTextInfo

    def script_sayLineOffsets(self, gesture):
        ti = self.makeTextInfo("caret")
        startOffset, endOffset = ti._getLineOffsets(ti._startOffset)
        ui.message(f"{ti._startOffset}, {startOffset}, {endOffset}")

    def script_sayVisibility(self, gesture):
        vis = rpcClient.request("pointInvisibleP")
        ui.message(f"{vis}")

    __gestures = {
        "kb:NVDA+L": "sayLineOffsets",
        "kb:NVDA+I": "sayVisibility",
    }


class AppModule(appModuleHandler.AppModule):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if not initClient():
            ui.browseableMessage("Eval server is probably not running.")
        else:
            # Register event handlers
            rpcClient.registerEventHandler("speak", self._onSpeakEvent)

    def _onSpeakEvent(self, data):
        """Handle speak event from Emacs."""
        import speech
        text = data.get("text", "")
        if text:
            speech.speakMessage(text)

    def terminate(self):
        global rpcClient
        super().terminate()
        if rpcClient:
            rpcClient.close()
        rpcClient = None
        tones.beep(500, 50)

    def chooseNVDAObjectOverlayClasses(self, obj, clsList):
        if obj.windowClassName == "Emacs":
            clsList.insert(0, Emacs)
