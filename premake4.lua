
-- vars
local win_unixenv = false
local cygwin = false
local mingw = false
local clang_libcxx = false
local gcc_compat = false
local windows_no_cmd = false
local platform = "x32"
local system_includes = ""

function add_include(path)
	system_includes = system_includes.." -isystem "..path
end

-------------------------------------------------------------------------------
-- basic config for all projects
solution "tccpp"
	-- scan args
	local argc = 1
	while(_ARGS[argc] ~= nil) do
		if(_ARGS[argc] == "--env") then
			argc=argc+1
			-- check if we are building with cygwin/mingw
			if(_ARGS[argc] ~= nil and _ARGS[argc] == "cygwin") then
				cygwin = true
				win_unixenv = true
			end
			if(_ARGS[argc] ~= nil and _ARGS[argc] == "mingw") then
				mingw = true
				win_unixenv = true
			end
		end
		if(_ARGS[argc] == "--clang") then
			clang_libcxx = true
		end
		if(_ARGS[argc] == "--gcc") then
			gcc_compat = true
		end
		if(_ARGS[argc] == "--platform") then
			argc=argc+1
			if(_ARGS[argc] ~= nil) then
				platform = _ARGS[argc]
			end
		end
		argc=argc+1
	end
	
	configurations { "Release", "Debug" }
	
	-- os specifics
	if(not os.is("windows") or win_unixenv) then
		if(not cygwin) then
			add_include("/usr/include")
		end
		add_include("/usr/local/include")
		buildoptions { "-Wall -std=c11" }
		
		if(clang_libcxx) then
			buildoptions { "-Weverything" }
			buildoptions { "-Wno-unknown-warning-option" }
			buildoptions { "-Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-header-hygiene -Wno-gnu -Wno-float-equal" }
			buildoptions { "-Wno-documentation -Wno-system-headers -Wno-global-constructors -Wno-padded -Wno-packed" }
			buildoptions { "-Wno-switch-enum -Wno-sign-conversion -Wno-conversion -Wno-exit-time-destructors" }
			linkoptions { "-fvisibility=default" }
		end
		
		if(gcc_compat) then
			buildoptions { "-Wno-trigraphs -Wreturn-type -Wunused-variable -Wno-strict-aliasing" }
		end
	end
	
	if(win_unixenv) then
		defines { "WIN_UNIXENV" }
		if(cygwin) then
			defines { "CYGWIN" }
		end
		if(mingw) then
			defines { "__WINDOWS__", "MINGW" }
			add_include("/mingw/include")
			libdirs { "/usr/lib", "/usr/local/lib" }
			buildoptions { "-Wno-unknown-pragmas" }
		end
	end
	
	if(os.is("linux") or os.is("bsd") or win_unixenv) then
		-- set system includes
		buildoptions { system_includes }
	end

	-- prefer system platform
	if(platform == "x64") then
		platforms { "x64", "x32" }
	else
		platforms { "x32", "x64" }
	end
	
	configuration { "x64" }
		defines { "PLATFORM_X64" }

	configuration { "x32" }
		defines { "PLATFORM_X86" }

-------------------------------------------------------------------------------
-- tccpp standalone executable
project "tccpp_standalone"
	targetname "tccpp"
	kind "ConsoleApp"
	language "C"
	files { "*.h", "*.c" }
	basedir "."
	targetdir "bin"

	includedirs { "." }

	configuration "Release"
		targetname "tccpp"
		defines { "NDEBUG" }
		flags { "Optimize" }
		if(not os.is("windows") or win_unixenv) then
			buildoptions { "-O3 -ffast-math" }
		end
		
	configuration "Debug"
		targetname "tccppd"
		defines { "DEBUG" }
		flags { "Symbols" }
		if(not os.is("windows") or win_unixenv) then
			buildoptions { "-gdwarf-2" }
		end
