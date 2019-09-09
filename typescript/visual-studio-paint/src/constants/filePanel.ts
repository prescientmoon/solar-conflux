import { IButtonConfig } from "../types/IButtonConfig"

export const filePanelButtonConfigs: IButtonConfig[] = [
  {
    icon: "create_new_folder",
    text: "create",
    action: () => {
      console.log("create")
    }
  },
  {
    icon: "folder_open",
    text: "open",
    action: () => {
      console.log("open")
    }
  },
  {
    icon: "cloud_download",
    text: "download",
    action: () => {
      console.log("download")
    }
  },
  {
    icon: "cloud_upload",
    text: "upolad",
    action: () => {
      console.log("upolad")
    }
  }
]
