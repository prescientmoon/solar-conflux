local M = {}

M.message_level = tonumber(settings.global["tws-interface-console-level"].value)
M.debug_log = settings.global["tws-interface-debug-logfile"].value

return M
