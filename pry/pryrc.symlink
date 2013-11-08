#!/usr/bin/env ruby
# Load plugins (only those I whitelist)
Pry.config.should_load_plugins = false
# Pry.plugins["doc"].activate!

Pry.config.editor = proc { |file, line| "vim +#{line} #{file}" }

# Launch Pry with access to the entire Rails stack.
# If you have Pry in your Gemfile, you can pass: ./script/console --irb=pry instead.
# If you don't, you can load it through the lines below :)
rails = File.join Dir.getwd, 'config', 'environment.rb'

# if File.exist?(rails) && ENV['SKIP_RAILS'].nil?
#   require rails

#   if Rails.version[0..0] == "2"
#     require 'console_app'
#     require 'console_with_helpers'
#   elsif Rails.version[0..0] == "3"
#     require 'rails/console/app'
#     require 'rails/console/helpers'
#   else
#     warn "[WARN] cannot load Rails console commands (Not on Rails2 or Rails3?)"
#   end
# end

if defined?(Rails) && Rails.env
  extend Rails::ConsoleMethods
end

class Object
  def vim method_name = nil
    if method_name.nil? && self.is_a?(Method)
      file, line = self.source_location
    else
      file, line = method(method_name).source_location
    end

    return system("tmux split-window -h \"vim #{file} +#{line}\"") if file

    puts "Source not available. Is this a C extension?"
  end
end

def table rows
  rows.each do |row|
    puts row
  end
end

class Object
  def method_lookup *args, result
    public_methods(false).select do |name|
      begin
        begin
          next clone.send(name, *args) == result
        rescue TypeError
          next send(name, *args) == result
        end

        send(name, *args) == result
      rescue
      end
    end
  end
end
