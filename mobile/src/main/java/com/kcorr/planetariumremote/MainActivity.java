package com.kcorr.planetariumremote;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothSocket;
import android.os.Build;
import android.os.Bundle;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.Snackbar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.DatePicker;
import android.widget.ImageButton;

import com.mhuss.AstroLib.AstroDate;
import com.mhuss.AstroLib.NoInitException;
import com.mhuss.AstroLib.ObsInfo;
import com.mhuss.AstroLib.PlanetData;
import com.mhuss.AstroLib.Planets;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

public class MainActivity extends AppCompatActivity {

    private final int[] PLANETS = {Planets.MERCURY, Planets.VENUS, Planets.EARTH};
    private final String TAG = "MainActivity";
    private final UUID BT_UUID = UUID.fromString("0000110E-0000-1000-8000-00805F9B34FB");
    private final String BT_REMOTE_MAC = ""; // TODO device discovery

    private ImageButton rewindButton;
    private ImageButton forwardButton;
    private DatePicker datePicker;
    private FloatingActionButton sendFab;

    private BluetoothAdapter btAdapter;
    private BluetoothDevice btDevice;
    private BluetoothSocket btSocket;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        this.rewindButton = (ImageButton) findViewById(R.id.rewind);
        this.forwardButton = (ImageButton) findViewById(R.id.forward);
        this.datePicker = (DatePicker) findViewById(R.id.datePicker);
        this.sendFab = (FloatingActionButton) findViewById(R.id.send);

        addClickListeners();

        // Connect to device
        new Thread(new Runnable() {
            public void run() {
                try {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2)
                        btAdapter = ((BluetoothManager) getSystemService(BLUETOOTH_SERVICE)).getAdapter();
                    else
                        btAdapter = BluetoothAdapter.getDefaultAdapter();
                    btDevice = btAdapter.getRemoteDevice(BT_REMOTE_MAC);
                    // TODO device discovery instead of hardcoded MAC address
                    btSocket = btDevice.createRfcommSocketToServiceRecord(BT_UUID);
                    btSocket.connect();
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            Snackbar.make(findViewById(R.id.main_activity_top), R.string.msg_connected,
                                    Snackbar.LENGTH_LONG).show();
                        }
                    });
                } catch (IOException e) {
                    Log.e(TAG, "IOException thrown while attempting to connect to device", e);
                    Snackbar.make(findViewById(R.id.main_activity_top), R.string.msg_connect_failed,
                            Snackbar.LENGTH_LONG);
                }
            }
        }).start();
    }

    private void addClickListeners() {

        this.rewindButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                datePicker.updateDate(datePicker.getYear() - 100,
                        datePicker.getMonth(), datePicker.getDayOfMonth());
            }
        });

        this.forwardButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                datePicker.updateDate(datePicker.getYear() + 100,
                        datePicker.getMonth(), datePicker.getDayOfMonth());
            }
        });

        this.sendFab.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                calcSendPositions();
            }
        });
    }

    private void calcSendPositions() {

        // Get positions from AstroLib
        double positions[][] = getHelioPositions(datePicker.getDayOfMonth(),
                datePicker.getMonth(), datePicker.getYear());
        if (BuildConfig.DEBUG) {
            String output = "Got these positions from AstroLib: [";
            for (int i=0; i<positions.length - 1; i++) {
                output += "[" + (int) positions[i][0] + ", " + positions[i][1] + ", "
                        + positions[i][2] + "], ";
            }
            int i = positions.length - 1;
            output += "[" + (int) positions[i][0] + ", " + positions[i][1] + ", "
                    + positions[i][2] + "]]";
            Log.d(TAG, output);
        }

        // Open an OutputStream
        OutputStream outStream;
        try {
            outStream = this.btSocket.getOutputStream();
        } catch (IOException e) {
            Log.e(TAG, "IOException thrown while attempting to open OutputStream to device", e);
            Snackbar.make(findViewById(R.id.main_activity_top), R.string.msg_connect_failed,
                    Snackbar.LENGTH_LONG);
            return;
        }

        // Send a line for each planet
        for (double planet[] : positions) {
            // Build payload
            String payload = "" + (int) planet[0] + " " + planet[1] + " " + planet[2] + "\n";
            if (BuildConfig.DEBUG)
                Log.d(TAG, "Sending this payload: \"" + payload + "\"");

            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
                    outStream.write(payload.getBytes(StandardCharsets.US_ASCII));
                else
                    outStream.write(payload.getBytes(Charset.forName("US-ASCII")));
            } catch (IOException e) {
                Log.e(TAG, "IOException thrown while attempting to send to OutputStream", e);
                Snackbar.make(findViewById(R.id.main_activity_top), R.string.msg_send_failed,
                        Snackbar.LENGTH_LONG);
                return;
            }
        }
    }


    private double[][] getHelioPositions(int day, int month, int year) {
        double result[][] = new double[this.PLANETS.length][3];

        double jd = new AstroDate(day, month, year).jd();
        ObsInfo info = new ObsInfo();
        PlanetData data = new PlanetData();

        for (int i=0; i<this.PLANETS.length; i++) {
            data.calc(this.PLANETS[i], jd, info);

            try {
                result[i][0] = this.PLANETS[i];
                result[i][1] = data.getPolarLon();
                result[i][2] = data.getPolarLat();
            } catch (NoInitException e) {
                Log.e(TAG, "PlanetData not initiated", e);
            }
        }

        return result;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
