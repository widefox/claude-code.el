# Multiple Instances Per Directory Feature

## Overview

I would like to extend claude-code.el's multiple instance feature to support multiple claude instances for the same directory / project, to support multi-Claude workflows. For example, within the ~/repos/claude-code.el project I would like to be able to run a default Claude instance to write source code, and another instance for writing unit tests. 

## Changes

I would like to make changes to the following commands:

### claude-code

- If no Claude instance is running for the project directory, start a new instance with the name 'default'. If the project directory is ~/repos/claude-code.el, then the claude buffer name for the new instance will be "*claude:~/respo/claude-code.el:default*".
- If there is a running Claude instance for the directory, prompt the user for a name for the new instance. If the user enters "tests", and the directory is ~/repos/claude-code.el, then the claude buffer name for the new instance will be "*claude:~/respo/claude-code.el:tests*". 

### Adjusting Other Commands

For commands like `claude-code-kill`, `claude-code-switch-to-buffer`, etc, if there are multiple instances running for the project directory, prompt the user for the instance. Currently this is done  via `claude-code--prompt-for-claude-buffer`.

Think about what  changes need to be made to achieve this.


