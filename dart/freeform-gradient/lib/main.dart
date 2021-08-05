import 'dart:math';

import 'package:flutter/material.dart';
import 'dart:ui' as ui;
import 'dart:async';
import 'dart:typed_data';
import 'package:vector_math/vector_math.dart' as vector;

void main() => runApp(Root());

class Root extends StatefulWidget {
  const Root({
    Key key,
  }) : super(key: key);

  @override
  RootState createState() => RootState();
}

class RootState extends State<Root> {
  double angle = 0;

  @override
  initState() {
    super.initState();

    Timer.periodic(Duration(milliseconds: 10), (timer) {
      setState(() {
        angle += 0.04;
      });
    });
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Custom gradient',
      theme: ThemeData(
        primarySwatch: Colors.purple,
      ),
      home: MyHomePage(
          title: 'Multi point gradient',
          config: MultiGradientConfig(
              distanceScaling: 1,
              blendStrength: 2,
              points: [
                MultiGradientPoint(
                  color: Colors.yellow,
                  intensity: 1.0,
                  position: vector.Vector2(
                      1.0 / 2 + cos(angle) * 0.3, 1.0 / 2 + sin(angle) * 0.3),
                ),
                MultiGradientPoint(
                    color: Color(0xff6E10AB),
                    intensity: 1,
                    position: vector.Vector2(0.1, 0.09)),
                MultiGradientPoint(
                    color: Color(0xff041E1C),
                    intensity: 2,
                    position: vector.Vector2(0.84, 0.14)),
                MultiGradientPoint(
                    color: Color(0xff015BBB),
                    intensity: 1,
                    position: vector.Vector2(0.07, 0.93)),
                MultiGradientPoint(
                    color: Color(0xff06A599),
                    intensity: 1,
                    position: vector.Vector2(0.9, 0.94))
              ])),
    );
  }
}

class MyHomePage extends StatelessWidget {
  MyHomePage({Key key, this.title, this.config}) : super(key: key);

  final String title;
  final MultiGradientConfig config;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
          child: FutureBuilder<ui.Image>(
        future: generateImage(MediaQuery.of(context).size, config),
        builder: (context, snapshot) {
          if (snapshot.hasData) {
            MediaQueryData query = MediaQuery.of(context);
            return CustomPaint(
                child: Container(
                  width: query.size.width,
                  height: query.size.height,
                ),
                painter: ImagePainter(image: snapshot.data));
          }

          return Text("Generating image");
        },
      )),
    );
  }
}

class ImagePainter extends CustomPainter {
  ui.Image image;

  ImagePainter({this.image});

  @override
  void paint(Canvas canvas, Size size) {
    canvas.drawImage(image, Offset.zero, Paint());
  }

  @override
  bool shouldRepaint(ImagePainter oldDelegate) {
    return oldDelegate.image != image;
  }
}

/// Generates a [ui.Image] with certain pixel data
Future<ui.Image> generateImage(Size size, MultiGradientConfig config) async {
  int width = size.width.ceil();
  int height = size.height.ceil();
  var completer = Completer<ui.Image>();

  Int32List pixels = Int32List(width * height);

  for (var x = 0; x < width; x++) {
    for (var y = 0; y < height; y++) {
      int index = y * width + x;
      pixels[index] = generatePixel(x, y, size, config);
    }
  }

  ui.decodeImageFromPixels(
    pixels.buffer.asUint8List(),
    width,
    height,
    ui.PixelFormat.bgra8888,
    (ui.Image img) {
      completer.complete(img);
    },
  );

  return completer.future;
}

class MultiGradientPoint {
  double intensity;
  vector.Vector4 color;
  vector.Vector2 position;

  MultiGradientPoint({Color color, this.position, this.intensity = 1}) {
    this.color = vector.Vector4(color.red.toDouble(), color.green.toDouble(),
        color.blue.toDouble(), color.opacity);
  }
}

class MultiGradientConfig {
  List<MultiGradientPoint> points;

  double distanceScaling;
  double blendStrength;
  double opacityMultiplier = 1;

  MultiGradientConfig(
      {this.points,
      this.distanceScaling = 1,
      this.blendStrength = 3,
      this.opacityMultiplier = 1});
}

double clamp(double min, double max, double value) {
  if (value < min) return min;
  if (value > max) return max;

  return value;
}

/// Main area of interest, this function will
/// return color for each particular color on our [ui.Image]
int generatePixel(int x, int y, Size size, MultiGradientConfig config) {
  vector.Vector4 color = vector.Vector4(0, 0, 0, 0);
  double scale = 0;

  var current = vector.Vector2(x.toDouble(), y.toDouble());

  current.divide(vector.Vector2(size.width, size.height));

  for (var i = 0; i < config.points.length; i++) {
    MultiGradientPoint point = config.points[i];

    var distance = pow(
        1 - point.position.distanceTo(current) / config.distanceScaling,
        config.blendStrength);
    var multiplier = distance * point.intensity;

    scale += multiplier;
    color += point.color.scaled(multiplier);
  }

  if (scale != 0) {
    color.scale(1 / scale);
  }

  return Color.fromRGBO(color.x.toInt(), color.y.toInt(), color.z.toInt(),
          color.w * config.opacityMultiplier)
      .value;
}
