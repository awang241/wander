<template>

    <div>
        <form>
            <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
        </form>
    </div>

</template>

<script>

    let autocompleteLocation;

    export default {
        name: "AutoCompleteLocation",
        data() {
            return {
                locationAddress : ""
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
                // eslint-disable-next-line no-undef
                autocompleteLocation = new google.maps.places.Autocomplete(document.getElementById("autocompleteLocation"), options)
                autocompleteLocation.setFields(['address_components']);
                autocompleteLocation.addListener('place_changed', () => {
                    var locationObject = autocompleteLocation.getPlace();
                    this.locationAddress = this.formatLocationTextField(locationObject);
                    document.getElementById("autocompleteLocation").value = this.locationAddress;
                    this.$parent.checkValidGeoCode(this.locationAddress)
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

            }

        },
        mounted() {
            this.initAutoCompleteLocation()
        }
    }
</script>

<style scoped>

</style>