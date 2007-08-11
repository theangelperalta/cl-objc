#import <Foundation/Foundation.h>
#import <Appkit/Appkit.h>

@interface AppDelegate : NSObject
{
}

- (void)applicationDidFinishLaunching: (id) aNotification;
- (void)sayHello:(id) sender;
@end

@implementation AppDelegate
- (void)applicationDidFinishLaunching: (id) aNotification
{
  printf("Hello, World!\n");
  fflush(0);
}

- (void)sayHello: (id) sender
{
  printf("Hello again, World!\n");
  fflush(0);
}
@end

int main() 
{
  id app = [NSApplication sharedApplication];
  id delegate = [[AppDelegate alloc] init];

  [NSApp setDelegate: delegate];
  
  id win = [NSWindow alloc];

  NSRect frame;
  frame.origin.x = 200.0;
  frame.origin.y = 300.0;
  frame.size.width = 250.0;
  frame.size.height = 100.0;
  
  [win initWithContentRect: frame styleMask: 15 backing: 2 defer: 0];
  [win setTitle: @"Hello World"];
  [win setLevel: 3];
  
  NSRect button_rect;
  button_rect.origin.x = 10.0;
  button_rect.origin.y = 10.0;
  button_rect.size.width = 80.0;
  button_rect.size.height = 80.0;

  id hel = [[NSButton alloc] initWithFrame: button_rect];
  [[win contentView] addSubview: hel];
  [hel setBezelStyle: 4];
  [hel setTitle: @"Hello"];
  [hel setTarget: [app delegate]];
  [hel setAction: @selector(sayHello:)];
  
  id beep = [NSSound alloc];
  [beep initWithContentsOfFile: @"/System/Library/Sounds/Tink.Aiff" byReference: 1];
  [hel setSound: beep];

  NSRect bye_rect;
  bye_rect.origin.x = 100.0;
  bye_rect.origin.y = 10.0;
  bye_rect.size.width = 80.0;
  bye_rect.size.height = 80.0;
  
  id bye = [[NSButton alloc] initWithFrame: bye_rect];
  [[win contentView] addSubview: bye];
  [bye setBezelStyle: 4];
  [bye setAction: @selector(stop:)];
  [bye setEnabled: 1];
  [bye setTitle: @"Goodbye!"];

  id adios = [NSSound alloc];
  [adios initWithContentsOfFile: @"/System/Library/Sounds/Basso.aiff" byReference: 1];
  [bye setSound: adios];

  [win display];
  [win orderFrontRegardless];

  [app run];
    
}
