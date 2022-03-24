OS=$(uname -s)

echo "OS=$OS"

[ "APPEND" != "1" ] && rm -f cabal.project.local

bmi_enabled() {
  case "$(uname -s)" in
    Darwin)
      sysctl -a | grep 'cpu\..*features' | grep BMI2 > /dev/null
      ;;
    Linux)
      cat /proc/cpuinfo | grep '^flags' | grep bmi2 > /dev/null
      ;;
    WinNT)
      return 0
      ;;
    *)
      echo "Unrecognised OS" 1>&2
      return 1
      ;;
  esac
}

(
  echo 'package bits-extra'
  bmi_enabled && echo ' flags: +bmi2'
  echo
  echo 'package hw-rankselect'
  bmi_enabled && echo 'flags: +bmi2'
  echo
  echo 'package hw-rankselect-base'
  bmi_enabled && echo 'flags: +bmi2'
  echo
  echo 'package hw-simd'
  bmi_enabled && echo 'flags: +bmi2'
  echo
) >> cabal.project.local
