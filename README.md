# acide-0-8-release-2010-2011

Automatically exported from *code.google.com/p/acide-0-8-release-2010-2011*

## Synopsis ##

This project is aimed to provide a free and cross-platform configurable integrated development environment which can be configured in order to be used with any development system such as interpreters, compilers and database systems.

Features of this system include: Project management, Multifile editing, Syntax colouring, Console panel, and Database panel (tested with DB2, MySQL, Oracle, PostgreSQL, SQL Anywhere and DES).

End-users who can benefit from this system include: console and database users, researchers that develop programming systems, and developers. It is implemented in Java in order to be platform independent.

The current release adds many new features and fixes bugs, but it remains to fix many others, remarkably the parsing-on-the-fly.

This tool is partially described in: _“ACIDE: An Integrated Development Environment Configurable for LaTeX”, PracTeX Journal 2007-3, special issue on “Tools for LaTeX and TeX Users”_.

### Release Notes ###
The new version has the following enhacements:
  - Source code in English and fully standardized.
  - new splash screen with the new logo of the application.
  - icons in all the menus of the application.
  - Display configuration options for the file editor and the console panel.
  - Lexicon and grammar configurations for each file opened in the file editor.
  - Default lexicon configurations for the files opened in the file editor based on their extension.
  - Lexicon configurations applied to the console panel.
  - Default lexicon configuration for the console panel. 
  - New menu configuration window.
  - Tool bar split into three different tool bars:
    - Tool bar based on menu options but not configurable.
    - Tool bar based on commands to be executed in the console panel.
    - Tool bar based on commands that execute external applications.
  - Extra parameter for the console panel commands: none, text, file, directory.
  - Console panel commands can also be executed in the OS shell and not in the console panel.
  - Scroll buttons for the tool bar panel to avoid command buttons' losts with the main window.
  - New tool bar configuration window with edition on the tables enabled.
  - Line wrapping in the file editors.
  - Automatic indents in the file editors.
  - Overwritting/Insert mode available in the file editor.
  - New line number component that highlights the current line in red and shows the exact number of 
  lines of the files opened in the file editor.
  - Focus in the text edition area when the user clicks on the tab.
  - Perfect coordination and synchronization of all the components in the main window of the application.
  - in Undo or Redo operations the focus goes automatically to the editor where the change belongs.
  - Perfect management of the close buttons in the file editor based on their modification state. 
  - Grammar creation process output window that displays the whole process.
  - Last opened file and project directory is stored to open the resources faster.
  - Compiler and Executable configurations are stored in the project configurations.
  - Errors in the configuration of ACIDE - A Configurable IDE do not collapse the application.
  - Multiselection in the opening and adding files operations.
  - Respect capitalization for the replacements.
  - Remarks format display options added in the lexicon configuration window.
  - Send the content of the active file editor to the console panel.
  - Maximum number of lines to send to the console.
  - The processes related to the loaded shells in the console panel are terminated when it corresponds.
  - Maximum buffer size definition for the console panel based on the number of lines.
  - Edition in the console panel has been disabled when if affects to text far beyond the prompt mark.
  - Searching operations in the console panel.
  - Reset the loaded shell in the console panel.
  - Close the loaded shell in the console panel.
  - Clear the buffer of the console panel.
  - Loaded shell processes in the console panel are killed and do not consume OS resources when the application or the loaded shells are closed.
  - Save the console panel content into an external file.
  - Open all the files related to the project into the file editor.
  - Add the opened files in the file editor to the current project.
  - Matching braces highlighting issues in the file editor have been fixed.
  - Lock keys detection updates the status bar in every component of the application.
  - New shortcuts have been added:
     - Mouse Wheel -> Scrolling line by line in the file editor and the console panel.
     - CONTROL+Mouse Wheel -> Zoom effect in the file editor and the console panel.
     - CONTROL+SHIFT+F3 -> Searching operation backwards in the file editor panel.
     - CONTROL+UP and DOWN -> Scrolling line by line in the file editor and the console panel.
  - ESCAPE key closes all the configuration windows of the application.
  
##Motivation##
Development of the ACIDE - A Configurable IDE 0.8 release for the final thesis of the alumn *Javier Salcedo Gómez* of the Faculty of Computer Science in The Complutense University of Madrid during the Academic Year 2010/2011

## Installation Notes ##

System Requirements:

  - Installation of *Java JRE 1.6* and later versions.
  - Installation of PDF reader software to visualize the help files.

## Running Notes ##

To run the application you have two options:
  - run the *acide.jar* file.
  - run the *launch.bat* file with some performance improvements related to memory and garbage collector matters.

## Developers Notes ##

The source code has been completely cleaned and refactored from the 0.7 version source code using *Eclipse IDE version 3.3.6*
for that purpose.

The project file for Eclipse 3.3.6 is also available with the current distribution of ACIDE - A Configurable IDE.
To start editing its source code in order to make your own distribution of ACIDE - A Configurable IDE you simply
have to import the project into your Eclipse IDE. 

For developers who are going to use Eclipse for the development of ACIDE:
The class com.thoughtworks.xstream.converters.reflection.Sun14ReflectionProvider is going to 
throw an error like The type [X] is not accessible due to restriction on required library [Y].

In order to fix it you will have to configure the compilance settings of Eclipse to set that
kind of exceptions like warnings and not as errors:
	--> Deprecated and restricted API		
	--> Forbidden reference --> Warning

The documentation of the source code in javadoc format is also available in the /doc folder of the present distribution folder. 
