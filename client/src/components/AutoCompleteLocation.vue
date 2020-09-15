<template>

    <div>
        <form>
            <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
        </form>
    </div>

</template>

<script>

    import googleMapsInit from "../utils/googlemaps";

    let autocompleteLocation;

    export default {
        name: "AutoCompleteLocation",
        props: ["profileLocation"],
        data() {
            return {
                location : {
                    address: "",
                    latitude: "",
                    longitude: ""
                },
                geocoder: "",
            }
        },
        methods: {

            /** This method sets up the autocomplete. It takes the location from the input field and reformats it to a single string.
             This string is saved to the DOM and will be sent to the backend
             There is a lot of logic within the add listener because Google Maps is not in the same scope as Vue. **/
            initAutoCompleteLocation() {
                let options = {
                    types: ['geocode'],
                };

                autocompleteLocation = new this.google.maps.places.Autocomplete(document.getElementById("autocompleteLocation"), options)
                autocompleteLocation.setFields(['address_components']);
                autocompleteLocation.addListener('place_changed', () => {
                    this.location.address = this.formatLocationTextField(autocompleteLocation.getPlace());
                    document.getElementById("autocompleteLocation").value = this.location.address;
                    this.checkValidGeoCode(this.location.address)
                })
            },

            formatLocationTextField(locationObject) {
                let locationString = "";
                for (let i = 0; i < (locationObject.address_components).length; i++) {
                    if (i === 0) {
                        locationString = locationObject.address_components[0].long_name;
                    } else if (i !== (locationObject.address_components).length) {
                        if (locationObject.address_components[i].long_name !== locationObject.address_components[i - 1].long_name) {
                            locationString = locationString + ", " + locationObject.address_components[i].long_name;
                        }
                    }
                }
                return locationString

            },

            checkValidGeoCode(locationAddress) {
                return new Promise((resolve, reject) => {
                    this.geocoder.geocode({'address': locationAddress}, (results, status) => {
                        if (status === 'OK') {
                            this.location.latitude = results[0].geometry.location.lat()
                            this.location.longitude = results[0].geometry.location.lng()
                            this.$parent.updateMapLocationFromAutoComplete(this.location);
                            resolve(true)
                        } else {
                            reject(false);
                        }
                    })
                })
            },

            async setLocation() {
                if (this.profileLocation.address != "") {
                    this.location.address = this.profileLocation.address;
                    document.getElementById("autocompleteLocation").value = this.location.address;
                    await this.checkValidGeoCode(this.location.address)
                    this.$parent.updateMapLocationFromAutoComplete(this.location);
                }
            },

            clearLocation() {
                this.location = {location: "", latitude: "", longitude: ""}
            },

            updateLocation(locationAddress) {
                this.location.address = locationAddress
                this.checkValidGeoCode(locationAddress)

            },

            returnLocation() {
                return this.location;
            }

        },
        async mounted() {
            this.google = await googleMapsInit();
            this.geocoder = new this.google.maps.Geocoder;
            this.initAutoCompleteLocation()
            this.setLocation()
        }
    }
</script>

<style scoped>

</style>