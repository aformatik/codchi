# GPU

::: tip
Try it out with `codchi init <NAME> https://github.com/aformatik/codchi nixosModules.gpu`.
:::

GPU usage should work by default inside a code machine. Codchi uses the GPU driver from your host and therefore only works when the driver is installed on your host. Currently only Nvidia/CUDA is tested.

## Nvidia

### Check if it works

```bash
codchi init gpu_test # create an empty machine
codchi exec gpu_test nvidia-smi
# Output should look similar to this:
+---------------------------------------------------------------------------------------+
| NVIDIA-SMI 535.157                Driver Version: 538.18       CUDA Version: 12.2     |
|-----------------------------------------+----------------------+----------------------+
| GPU  Name                 Persistence-M | Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp   Perf          Pwr:Usage/Cap |         Memory-Usage | GPU-Util  Compute M. |
|                                         |                      |               MIG M. |
|=========================================+======================+======================|
|   0  Quadro P500                    On  | 00000000:02:00.0 Off |                  N/A |
| N/A   55C    P0              N/A / ERR! |      0MiB /  2048MiB |      1%      Default |
|                                         |                      |                  N/A |
+-----------------------------------------+----------------------+----------------------+
                                                                                         
+---------------------------------------------------------------------------------------+
| Processes:                                                                            |
|  GPU   GI   CI        PID   Type   Process name                            GPU Memory |
|        ID   ID                                                             Usage      |
|=======================================================================================|
|  No running processes found                                                           |
+---------------------------------------------------------------------------------------+
```

### nvidia-container-toolkit

To use your GPU inside Docker inside a code machine, enable `codchi.docker.enableNvidia`. This also requires requires `nixpkgs.config.allowUnfree = true`.

For example, here is how you can run [InvokeAI](https://github.com/invoke-ai/InvokeAI) inside a code machine:
```bash
codchi init invokeai  https://github.com/aformatik/codchi nixosModules.gpu
codchi exec invokeai
# on NixOS>=24.11
docker run --runtime=nvidia --device nvidia.com/gpu=all --publish 9090:9090 ghcr.io/invoke-ai/invokeai
# on NixOS<24.11
docker run --runtime=nvidia --gpus=all --publish 9090:9090 ghcr.io/invoke-ai/invokeai
# open web ui in your windows browser (in another terminal)
xdg-open "http://$(ip route | awk '/^default/{print $3; exit}'):9090"
```
